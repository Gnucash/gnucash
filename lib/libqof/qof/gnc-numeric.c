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

#include "config.h"

#include <glib.h>
#include <math.h>
#ifdef G_OS_WIN32
#include <pow.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gnc-numeric.h"
#include "qofmath128.c"

/* static short module = MOD_ENGINE; */

/* =============================================================== */

#if 0
static const char * _numeric_error_strings[] = 
{
  "No error",
  "Argument is not a valid number",
  "Intermediate result overflow",
  "Argument denominators differ in GNC_HOW_DENOM_FIXED operation",
  "Remainder part in GNC_HOW_RND_NEVER operation"
};
#endif

/* =============================================================== */
/* This function is small, simple, and used everywhere below, 
 * lets try to inline it.
 */
inline GNCNumericErrorCode
gnc_numeric_check(gnc_numeric in) 
{
  if(in.denom != 0) 
  {
    return GNC_ERROR_OK;
  }
  else if(in.num) 
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

/*
 *  Find the least common multiple of the denominators of a and b.
 */

static inline gint64
gnc_numeric_lcd(gnc_numeric a, gnc_numeric b) 
{
  qofint128 lcm;
  if(gnc_numeric_check(a) || gnc_numeric_check(b)) 
  {
    return GNC_ERROR_ARG;
  }

  if (b.denom == a.denom) return a.denom;
  
  /* Special case: smaller divides smoothly into larger */
  if ((b.denom < a.denom) && ((a.denom % b.denom) == 0))
  {
    return a.denom;
  }
  if ((a.denom < b.denom) && ((b.denom % a.denom) == 0))
  {
    return b.denom;
  }

  lcm = lcm128 (a.denom, b.denom);
  if (lcm.isbig) return GNC_ERROR_ARG;
  return lcm.lo;
}


/* Return the ratio n/d reduced so that there are no common factors. */
static inline gnc_numeric
reduce128(qofint128 n, gint64 d)
{
  gint64   t;
  gint64   num;
  gint64   denom;
  gnc_numeric out;
  qofint128 red;

  t =  rem128 (n, d);
  num = d;
  denom = t;

  /* The strategy is to use Euclid's algorithm */
  while (denom > 0) 
  {
    t = num % denom;
    num = denom;
    denom = t;
  }
  /* num now holds the GCD (Greatest Common Divisor) */

  red = div128 (n, num);
  if (red.isbig)
  {
    return gnc_numeric_error (GNC_ERROR_OVERFLOW);
  }
  out.num   = red.lo;
  if (red.isneg) out.num = -out.num;
  out.denom = d / num;
  return out;
}

/* *******************************************************************
 *  gnc_numeric_zero_p
 ********************************************************************/

gboolean
gnc_numeric_zero_p(gnc_numeric a) 
{
  if(gnc_numeric_check(a)) 
  {
    return 0;
  }
  else 
  {
    if((a.num == 0) && (a.denom != 0)) 
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
  if(gnc_numeric_check(a)) 
  {
    return 0;
  }
  else 
  {
    if((a.num < 0) && (a.denom != 0)) 
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
  if(gnc_numeric_check(a)) 
  {
    return 0;
  }
  else 
  {
    if((a.num > 0) && (a.denom != 0)) 
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
  qofint128 l, r;

  if(gnc_numeric_check(a) || gnc_numeric_check(b)) 
  {
    return 0;
  }

  if (a.denom == b.denom)
  {
    if(a.num == b.num) return 0;
    if(a.num > b.num) return 1;
    return -1;
  }

  if  ((a.denom > 0) && (b.denom > 0))
  {
    /* Avoid overflows using 128-bit intermediate math */
    l = mult128 (a.num, b.denom);
    r = mult128 (b.num, a.denom);
    return cmp128 (l,r);
  }

  if (a.denom < 0)
      a.denom *= -1;
  if (b.denom < 0)
      b.denom *= -1;

  /* BUG: Possible overflow here..  Also, doesn't properly deal with
   * reciprocal denominators.
   */
  aa = a.num * a.denom;
  bb = b.num * b.denom;

  if(aa == bb) return 0;
  if(aa > bb) return 1;
  return -1;
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
  qofint128 l, r;
  if ((a.denom == b.denom) && (a.denom > 0))
  {
    return (a.num == b.num);
  }
  if ((a.denom > 0) && (b.denom > 0))
  {
    // return (a.num*b.denom == b.num*a.denom);
    l = mult128 (a.num, b.denom);
    r = mult128 (b.num, a.denom);
    return equal128 (l, r);

#if ALT_WAY_OF_CHECKING_EQUALITY
    gnc_numeric ra = gnc_numeric_reduce (a);
    gnc_numeric rb = gnc_numeric_reduce (b);
    if (ra.denom != rb.denom) return 0;
    if (ra.num != rb.num) return 0;
    return 1;
#endif
  }
  if ((a.denom < 0) && (b.denom < 0)) {
      l = mult128 (a.num, -a.denom);
      r = mult128 (b.num, -b.denom);
      return equal128 (l, r);
  } else {
      /* BUG: One of the numbers has a reciprocal denom, and the other
         does not. I just don't know to handle this case in any
         reasonably overflow-proof yet simple way.  So, this funtion
         will simply get it wrong whenever the three multiplies
         overflow 64-bits.  -CAS */
      if (a.denom < 0) {
          return ((a.num * -a.denom * b.denom) == b.num);
      } else {
          return (a.num == (b.num * a.denom * -b.denom));
      }
  }

  return ((a.num * b.denom) == (a.denom * b.num));
}


/* *******************************************************************
 *  gnc_numeric_same
 *  would a and b be equal() if they were both converted to the same 
 *  denominator? 
 ********************************************************************/

int
gnc_numeric_same(gnc_numeric a, gnc_numeric b, gint64 denom, 
                 gint how) {
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
  gnc_numeric sum;
  
  if(gnc_numeric_check(a) || gnc_numeric_check(b)) 
  {
    return gnc_numeric_error(GNC_ERROR_ARG);
  }

  if((denom == GNC_DENOM_AUTO) && 
     (how & GNC_NUMERIC_DENOM_MASK) == GNC_HOW_DENOM_FIXED) 
  {
    if(a.denom == b.denom) {
      denom = a.denom;
    }
    else if(b.num == 0) {
      denom = a.denom;
      b.denom = a.denom;
    }
    else if(a.num == 0) {
      denom = b.denom;
      a.denom = b.denom;
    }
    else {
      return gnc_numeric_error(GNC_ERROR_DENOM_DIFF);
    }
  }
  
  if(a.denom < 0) 
  {
    a.num *= -a.denom;  /* BUG: overflow not handled.  */
    a.denom = 1;
  }

  if(b.denom < 0) 
  {
    b.num *= -b.denom;  /* BUG: overflow not handled.  */
    b.denom = 1;
  }

  /* Get an exact answer.. same denominator is the common case. */ 
  if(a.denom == b.denom) 
  {
    sum.num = a.num + b.num;  /* BUG: overflow not handled.  */
    sum.denom = a.denom;
  }
  else 
  {
    /* We want to do this:
     *    sum.num = a.num*b.denom + b.num*a.denom;
     *    sum.denom = a.denom*b.denom;
     * but the multiply could overflow.  
     * Computing the LCD minimizes likelihood of overflow
     */
    gint64 lcd;
    qofint128 ca, cb, cab;
    lcd = gnc_numeric_lcd(a,b);
    if (GNC_ERROR_ARG == lcd)
    {
       return gnc_numeric_error(GNC_ERROR_OVERFLOW);
    }
    ca = mult128 (a.num, lcd/a.denom);
    if (ca.isbig) return gnc_numeric_error(GNC_ERROR_OVERFLOW);

    cb = mult128 (b.num, lcd/b.denom);
    if (cb.isbig) return gnc_numeric_error(GNC_ERROR_OVERFLOW);

    cab = add128 (ca, cb);
    if (cab.isbig) return gnc_numeric_error(GNC_ERROR_OVERFLOW);
    
    sum.num   = cab.lo;
    if (cab.isneg) sum.num = -sum.num;
    sum.denom = lcd;
  }
  
  if((denom == GNC_DENOM_AUTO) &&
     ((how & GNC_NUMERIC_DENOM_MASK) == GNC_HOW_DENOM_LCD)) 
  {
    denom = gnc_numeric_lcd(a, b);
    how   = how & GNC_NUMERIC_RND_MASK;
  }
  
  return gnc_numeric_convert(sum, denom, how);                             
}

/* *******************************************************************
 *  gnc_numeric_sub
 ********************************************************************/

gnc_numeric
gnc_numeric_sub(gnc_numeric a, gnc_numeric b, 
                gint64 denom, gint how) 
{
  gnc_numeric nb;
  if(gnc_numeric_check(a) || gnc_numeric_check(b)) 
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
  gnc_numeric product, result;
  qofint128 bignume, bigdeno;
  
  if(gnc_numeric_check(a) || gnc_numeric_check(b)) {
    return gnc_numeric_error(GNC_ERROR_ARG);
  }

  if((denom == GNC_DENOM_AUTO) && 
     (how & GNC_NUMERIC_DENOM_MASK) == GNC_HOW_DENOM_FIXED) {
    if(a.denom == b.denom) {
      denom = a.denom;
    }
    else if(b.num == 0) {
      denom = a.denom;
    }
    else if(a.num == 0) {
      denom = b.denom;
    }
    else {
      return gnc_numeric_error(GNC_ERROR_DENOM_DIFF);
    }
  }

  if((denom == GNC_DENOM_AUTO) &&
     ((how & GNC_NUMERIC_DENOM_MASK) == GNC_HOW_DENOM_LCD)) 
  {
    denom = gnc_numeric_lcd(a, b);
    how   = how & GNC_NUMERIC_RND_MASK;
  }

  if(a.denom < 0) {
    a.num *= -a.denom;  /* BUG: overflow not handled.  */
    a.denom = 1;
  }

  if(b.denom < 0) {
    b.num *= -b.denom;  /* BUG: overflow not handled.  */
    b.denom = 1;
  }

  bignume = mult128 (a.num, b.num);
  bigdeno = mult128 (a.denom, b.denom);
  product.num   = a.num*b.num;
  product.denom = a.denom*b.denom;

  /* If it looks to be overflowing, try to reduce the fraction ... */
  if (bignume.isbig || bigdeno.isbig)
  {
    gint64 tmp;
    a = gnc_numeric_reduce (a);
    b = gnc_numeric_reduce (b);
    tmp = a.num;
    a.num = b.num;
    b.num = tmp;
    a = gnc_numeric_reduce (a);
    b = gnc_numeric_reduce (b);

    bignume = mult128 (a.num, b.num);
    bigdeno = mult128 (a.denom, b.denom);
    product.num   = a.num*b.num;
    product.denom = a.denom*b.denom;
  }

  /* If it its still overflowing, and rounding is allowed then round */
  if (bignume.isbig || bigdeno.isbig)
  {
    /* If rounding allowed, then shift until there's no 
     * more overflow. The conversion at the end will fix 
     * things up for the final value. Else overflow. */
    if ((how & GNC_NUMERIC_RND_MASK) == GNC_HOW_RND_NEVER)
    {
      if (bigdeno.isbig)
      {
        return gnc_numeric_error (GNC_ERROR_OVERFLOW);
      }
      product = reduce128 (bignume, product.denom);
      if (gnc_numeric_check (product))
      {
        return gnc_numeric_error (GNC_ERROR_OVERFLOW);
      }
    } 
    else 
    {
      while (bignume.isbig || bigdeno.isbig)
      {
         bignume = shift128 (bignume);
         bigdeno = shift128 (bigdeno);
      }
      product.num = bignume.lo;
      if (bignume.isneg) product.num = -product.num;

      product.denom = bigdeno.lo;
      if (0 == product.denom) 
      {
        return gnc_numeric_error (GNC_ERROR_OVERFLOW);
      }
    }
  }
  
#if 0  /* currently, product denom won't ever be zero */
  if(product.denom < 0) {
    product.num   = -product.num;
    product.denom = -product.denom;
  }
#endif
  
  result = gnc_numeric_convert(product, denom, how);                             
  return result;
}


/* *******************************************************************
 *  gnc_numeric_div
 ********************************************************************/

gnc_numeric
gnc_numeric_div(gnc_numeric a, gnc_numeric b, 
                gint64 denom, gint how) 
{
  gnc_numeric quotient;
  qofint128 nume, deno;

  if(gnc_numeric_check(a) || gnc_numeric_check(b)) 
  {
    return gnc_numeric_error(GNC_ERROR_ARG);
  }

  if((denom == GNC_DENOM_AUTO) && 
     (how & GNC_NUMERIC_DENOM_MASK) == GNC_HOW_DENOM_FIXED) 
  {
    if(a.denom == b.denom) 
    {
      denom = a.denom;
    }
    else if(a.denom == 0) 
    {
      denom = b.denom;
    }
    else 
    {
      return gnc_numeric_error(GNC_ERROR_DENOM_DIFF);
    }
  }
  

  if(a.denom < 0) 
  {
    a.num *= -a.denom;   /* BUG: overflow not handled.  */
    a.denom = 1;
  }

  if(b.denom < 0) 
  {
    b.num *= -b.denom;   /* BUG: overflow not handled.  */
    b.denom = 1;
  }

  if(a.denom == b.denom) 
  {
    quotient.num = a.num;
    quotient.denom = b.num;
  }
  else 
  {
    gint64 sgn = 1;
    if (0 > a.num)
    {
      sgn = -sgn;
      a.num = -a.num;
    }
    if (0 > b.num)
    {
      sgn = -sgn;
      b.num = -b.num;
    }
    nume = mult128(a.num, b.denom);
    deno = mult128(b.num, a.denom);

    /* Try to avoid overflow by removing common factors */
    if (nume.isbig && deno.isbig)
    {
      gnc_numeric ra = gnc_numeric_reduce (a);
      gnc_numeric rb = gnc_numeric_reduce (b);

      gint64 gcf_nume = gcf64(ra.num, rb.num);
      gint64 gcf_deno = gcf64(rb.denom, ra.denom);
      nume = mult128(ra.num/gcf_nume, rb.denom/gcf_deno);
      deno = mult128(rb.num/gcf_nume, ra.denom/gcf_deno);
    }

    if ((0 == nume.isbig) && (0 == deno.isbig))
    {
      quotient.num = sgn * nume.lo;
      quotient.denom = deno.lo;
      goto dive_done;
    }
    else if (0 == deno.isbig)
    {
      quotient = reduce128 (nume, deno.lo);
      if (0 == gnc_numeric_check (quotient))
      {
        quotient.num *= sgn;
        goto dive_done;
      }
    }

    /* If rounding allowed, then shift until there's no 
     * more overflow. The conversion at the end will fix 
     * things up for the final value. */
    if ((how & GNC_NUMERIC_RND_MASK) == GNC_HOW_RND_NEVER)
    {
      return gnc_numeric_error (GNC_ERROR_OVERFLOW);
    }
    while (nume.isbig || deno.isbig)
    {
       nume = shift128 (nume);
       deno = shift128 (deno);
    }
    quotient.num = sgn * nume.lo;
    quotient.denom = deno.lo;
    if (0 == quotient.denom) 
    {
      return gnc_numeric_error (GNC_ERROR_OVERFLOW);
    }
  }
  
  if(quotient.denom < 0) 
  {
    quotient.num   = -quotient.num;
    quotient.denom = -quotient.denom;
  }
  
dive_done:
  if((denom == GNC_DENOM_AUTO) &&
     ((how & GNC_NUMERIC_DENOM_MASK) == GNC_HOW_DENOM_LCD)) 
  {
    denom = gnc_numeric_lcd(a, b);
    how   = how & GNC_NUMERIC_RND_MASK;
  }

  return gnc_numeric_convert(quotient, denom, how); 
}
 
/* *******************************************************************
 *  gnc_numeric_neg
 *  negate the argument 
 ********************************************************************/

gnc_numeric
gnc_numeric_neg(gnc_numeric a) {
  if(gnc_numeric_check(a)) {
    return gnc_numeric_error(GNC_ERROR_ARG);
  }
  return gnc_numeric_create(- a.num, a.denom);
}

/* *******************************************************************
 *  gnc_numeric_neg
 *  return the absolute value of the argument 
 ********************************************************************/

gnc_numeric
gnc_numeric_abs(gnc_numeric a) 
{
  if(gnc_numeric_check(a)) {
    return gnc_numeric_error(GNC_ERROR_ARG);
  }
  return gnc_numeric_create(ABS(a.num), a.denom);
}

/* *******************************************************************
 *  gnc_numeric_convert
 ********************************************************************/

gnc_numeric
gnc_numeric_convert(gnc_numeric in, gint64 denom, gint how) 
{
  gnc_numeric out;
  gnc_numeric temp;
  gint64      temp_bc;
  gint64      temp_a;
  gint64      remainder;  
  gint64      sign;
  gint        denom_neg=0;
  double      ratio, logratio;
  double      sigfigs;
  qofint128 nume, newm;

  temp.num   = 0;
  temp.denom = 0;

  if(gnc_numeric_check(in)) {
    return gnc_numeric_error(GNC_ERROR_ARG);
  }
  
  if(denom == GNC_DENOM_AUTO) 
  {
    switch(how & GNC_NUMERIC_DENOM_MASK) 
    {
    default:
    case GNC_HOW_DENOM_LCD:   /* LCD is meaningless with AUTO in here */
    case GNC_HOW_DENOM_EXACT:
      return in;
      break;
      
    case GNC_HOW_DENOM_REDUCE:
      /* reduce the input to a relatively-prime fraction */
      return gnc_numeric_reduce(in);
      break;
      
    case GNC_HOW_DENOM_FIXED:
      if(in.denom != denom) {
        return gnc_numeric_error(GNC_ERROR_DENOM_DIFF);
      }
      else {
        return in;
      }
      break;
      
    case GNC_HOW_DENOM_SIGFIG:
      ratio    = fabs(gnc_numeric_to_double(in));
      if(ratio < 10e-20) {
        logratio = 0;
      }
      else {
        logratio = log10(ratio);
        logratio = ((logratio > 0.0) ? 
                    (floor(logratio)+1.0) : (ceil(logratio)));
      }
      sigfigs  = GNC_HOW_GET_SIGFIGS(how);

      if (fabs(sigfigs - logratio) > 18)
          return gnc_numeric_error(GNC_ERROR_OVERFLOW);

      if(sigfigs-logratio >= 0) {
        denom    = (gint64)(pow(10, sigfigs-logratio));
      }
      else {
        denom    = -((gint64)(pow(10, logratio-sigfigs)));
      }
      
      how = how & ~GNC_HOW_DENOM_SIGFIG & ~GNC_NUMERIC_SIGFIGS_MASK;
      break;

    }
  }
  
  /* Make sure we need to do the work */
  if(in.denom == denom) {
    return in;
  }
  if(in.num == 0) {
    out.num = 0;
    out.denom = denom;
    return out;
  }
  
  /* If the denominator of the input value is negative, get rid of that. */
  if(in.denom < 0) {
    in.num = in.num * (- in.denom);  /* BUG: overflow not handled.  */
    in.denom = 1;
  }
  
  sign = (in.num < 0) ? -1 : 1;

  /* If the denominator is less than zero, we are to interpret it as 
   * the reciprocal of its magnitude. */
  if(denom < 0) 
  {

    /* XXX FIXME: use 128-bit math here ... */
    denom     = - denom;
    denom_neg = 1;
    temp_a    = (in.num < 0) ? -in.num : in.num;
    temp_bc   = in.denom * denom;  /* BUG: overflow not handled.  */
    remainder = temp_a % temp_bc;
    out.num   = temp_a / temp_bc;
    out.denom = - denom;    
  }
  else 
  {
    /* Do all the modulo and int division on positive values to make
     * things a little clearer. Reduce the fraction denom/in.denom to
     * help with range errors */
    temp.num   = denom;
    temp.denom = in.denom;
    temp       = gnc_numeric_reduce(temp);

    /* Symbolically, do the following:
     * out.num   = in.num * temp.num;
     * remainder = out.num % temp.denom;
     * out.num   = out.num / temp.denom;
     * out.denom = denom;
     */
    nume = mult128 (in.num, temp.num);
    newm = div128 (nume, temp.denom);
    remainder = rem128 (nume, temp.denom);

    if (newm.isbig)
    {
       return gnc_numeric_error(GNC_ERROR_OVERFLOW);
    }

    out.num = newm.lo;
    out.denom = denom;
  }

  if (remainder) 
  {
    switch(how & GNC_NUMERIC_RND_MASK) 
    {
    case GNC_HOW_RND_FLOOR:
      if(sign < 0) {
        out.num = out.num + 1;
      }
      break;
      
    case GNC_HOW_RND_CEIL:
      if(sign > 0) {
        out.num = out.num + 1;
      }
      break;
      
    case GNC_HOW_RND_TRUNC:
      break;

    case GNC_HOW_RND_PROMOTE:
      out.num = out.num + 1;
      break;
      
    case GNC_HOW_RND_ROUND_HALF_DOWN:
      if(denom_neg) 
      {
        if((2 * remainder) > in.denom*denom) 
        {
          out.num = out.num + 1;
        }
      }
      else if((2 * remainder) > temp.denom) 
      {
        out.num = out.num + 1;
      }
      /* check that 2*remainder didn't over-flow */
      else if (((2 * remainder) < remainder) && 
               (remainder > (temp.denom / 2)))
      {
        out.num = out.num + 1;
      }
      break;
      
    case GNC_HOW_RND_ROUND_HALF_UP:
      if(denom_neg) 
      {
        if((2 * remainder) >= in.denom*denom) 
        {
          out.num = out.num + 1;
        }
      }
      else if((2 * remainder ) >= temp.denom) 
      {
        out.num = out.num + 1;
      }
      /* check that 2*remainder didn't over-flow */
      else if (((2 * remainder) < remainder) && 
               (remainder >= (temp.denom / 2)))
      {
        out.num = out.num + 1;
      }
      break;
      
    case GNC_HOW_RND_ROUND:
      if(denom_neg) 
      {
        if((2 * remainder) > in.denom*denom) 
        {
          out.num = out.num + 1;
        }
        else if((2 * remainder) == in.denom*denom) 
        {
          if(out.num % 2) 
          {
            out.num = out.num + 1;
          }
        }        
      }
      else 
      {
        if((2 * remainder ) > temp.denom) 
        {
          out.num = out.num + 1;
        }
        /* check that 2*remainder didn't over-flow */
        else if (((2 * remainder) < remainder) && 
                 (remainder > (temp.denom / 2)))
        {
          out.num = out.num + 1;
        }
        else if((2 * remainder) == temp.denom) 
        {
          if(out.num % 2) 
          {
            out.num = out.num + 1;
          }
        }    
        /* check that 2*remainder didn't over-flow */
        else if (((2 * remainder) < remainder) && 
                 (remainder ==  (temp.denom / 2)))
        {
          if(out.num % 2) 
          {
            out.num = out.num + 1;
          }
        }
      }    
      break;
      
    case GNC_HOW_RND_NEVER:
      return gnc_numeric_error(GNC_ERROR_REMAINDER);
      break;
    }
  }
  
  out.num = (sign > 0) ? out.num : (-out.num);
  
  return out;
}


/* *******************************************************************
 *  reduce a fraction by GCF elimination.  This is NOT done as a
 *  part of the arithmetic API unless GNC_HOW_DENOM_REDUCE is specified 
 *  as the output denominator.
 ********************************************************************/

gnc_numeric
gnc_numeric_reduce(gnc_numeric in) 
{
  gint64   t;
  gint64   num = (in.num < 0) ? (- in.num) : in.num ;
  gint64   denom = in.denom;
  gnc_numeric out;

  if(gnc_numeric_check(in)) 
  {
    return gnc_numeric_error(GNC_ERROR_ARG);
  }

  /* The strategy is to use Euclid's algorithm */
  while (denom > 0) {
    t = num % denom;
    num = denom;
    denom = t;
  }
  /* num now holds the GCD (Greatest Common Divisor) */

  /* All calculations are done on positive num, since it's not 
   * well defined what % does for negative values */
  out.num   = in.num / num;
  out.denom = in.denom / num;
  return out;
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
    converted_val = gnc_numeric_convert(converted_val, 1, GNC_DENOM_EXACT);
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

gnc_numeric
double_to_gnc_numeric(double in, gint64 denom, gint how) 
{
  gnc_numeric out;
  gint64 int_part=0;
  double frac_part;
  gint64 frac_int=0;
  double logval; 
  double sigfigs;

  if((denom == GNC_DENOM_AUTO) && (how & GNC_HOW_DENOM_SIGFIG)) 
  {
    if(fabs(in) < 10e-20) {
      logval = 0;
    }
    else {
      logval   = log10(fabs(in));
      logval   = ((logval > 0.0) ? 
                  (floor(logval)+1.0) : (ceil(logval)));
    }
    sigfigs  = GNC_HOW_GET_SIGFIGS(how);
    if(sigfigs-logval >= 0) {
      denom    = (gint64)(pow(10, sigfigs-logval));
    }
    else {
      denom    = -((gint64)(pow(10, logval-sigfigs)));
    }

    how =  how & ~GNC_HOW_DENOM_SIGFIG & ~GNC_NUMERIC_SIGFIGS_MASK;
  }

  int_part  = (gint64)(floor(fabs(in)));
  frac_part = in - (double)int_part;
  
  int_part = int_part * denom;
  frac_part = frac_part * (double)denom;

  switch(how & GNC_NUMERIC_RND_MASK) {
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
    if(frac_part != (double) frac_int) {
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
  if(in.denom > 0) 
  {
    return (double)in.num/(double)in.denom;
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

  if(error) {
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
  if(error) {
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
  if(error) {
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
  if(error) {
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

  p+= 100;
  if (p-buff >= 1000) p = buff;
   
  sprintf(p, "%" G_GINT64_FORMAT "/%" G_GINT64_FORMAT, tmpnum, tmpdenom);

  return p;
}

gboolean
string_to_gnc_numeric(const gchar* str, gnc_numeric *n) 
{
  size_t num_read;
  gint64 tmpnum;
  gint64 tmpdenom;
    
  if(!str) return FALSE;

#ifdef GNC_DEPRECATED
  /* must use "<" here because %n's effects aren't well defined */
  if(sscanf(str, " " GNC_SCANF_LLD "/" GNC_SCANF_LLD "%n",
            &tmpnum, &tmpdenom, &num_read) < 2) {
    return FALSE;
  }
#else
  tmpnum = strtoll (str, NULL, 0);
  str = strchr (str, '/');
  if (!str) return FALSE;
  str ++;
  tmpdenom = strtoll (str, NULL, 0);
  num_read = strspn (str, "0123456789");
#endif
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

	newvalue = g_malloc( sizeof( gnc_numeric ) );
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

	if( type == 0 ) {
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
  if(gnc_numeric_check(in)) {
    retval = g_strdup_printf("<ERROR> [%" G_GINT64_FORMAT " / %" G_GINT64_FORMAT "]",
                             in.num,
                             in.denom); 
  }
  else {
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

/* ======================== END OF FILE =================== */
