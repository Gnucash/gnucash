/********************************************************************
 * gnc-numeric.c -- an exact-number library for gnucash.            *
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

#define _GNU_SOURCE

#include <glib.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>

#include "gnc-engine-util.h"
#include "gnc-numeric.h"

/* TODO 
 * - use longer intermediate values to make operations
 *   64-bit-overflow-proof 
 */

/* static short module = MOD_ENGINE; */

static const char * _numeric_error_strings[] = 
{
  "No error",
  "Argument is not a valid number",
  "Intermediate result overflow",
  "Argument denominators differ in GNC_DENOM_FIXED operation",
  "Remainder part in GNC_RND_NEVER operation"
};

static gint64 gnc_numeric_lcd(gnc_numeric a, gnc_numeric b);

/********************************************************************
 *  gnc_numeric_zero_p
 ********************************************************************/

int
gnc_numeric_zero_p(gnc_numeric a) {
  if(gnc_numeric_check(a)) {
    return 0;
  }
  else {
    if((a.num == 0) && (a.denom != 0)) {
      return 1;
    }
    else {
      return 0;
    }
  }
}

/********************************************************************
 *  gnc_numeric_negative_p
 ********************************************************************/

int
gnc_numeric_negative_p(gnc_numeric a) {
  if(gnc_numeric_check(a)) {
    return 0;
  }
  else {
    if((a.num < 0) && (a.denom != 0)) {
      return 1;
    }
    else {
      return 0;
    }
  }
}

/********************************************************************
 *  gnc_numeric_positive_p
 ********************************************************************/

int
gnc_numeric_positive_p(gnc_numeric a) {
  if(gnc_numeric_check(a)) {
    return 0;
  }
  else {
    if((a.num > 0) && (a.denom != 0)) {
      return 1;
    }
    else {
      return 0;
    }
  }
}


/********************************************************************
 *  gnc_numeric_compare
 *  returns 1 if a>b, -1 if b>a, 0 if a == b 
 ********************************************************************/

int
gnc_numeric_compare(gnc_numeric a, gnc_numeric b) {
  gint64 ab, ba;

  if(gnc_numeric_check(a) || gnc_numeric_check(b)) {
    return 0;
  }
  ab = a.num * b.denom;
  ba = b.num * a.denom;

  if(ab == ba) {
    return 0;
  }
  else if(ab > ba) {
    return 1;
  }
  else {
    return -1;
  }
}


/********************************************************************
 *  gnc_numeric_eq
 ********************************************************************/

int
gnc_numeric_eq(gnc_numeric a, gnc_numeric b) {
  return ((a.num == b.num) && (a.denom == b.denom));
}


/********************************************************************
 *  gnc_numeric_equal
 ********************************************************************/

int
gnc_numeric_equal(gnc_numeric a, gnc_numeric b) {
  if(((a.denom > 0) && (b.denom > 0)) ||
     ((a.denom < 0) && (b.denom < 0))) {    
    return ((a.num * b.denom) == (a.denom * b.num));
  }
  else {
    return 0;
  }
}


/********************************************************************
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



/********************************************************************
 *  gnc_numeric_add
 ********************************************************************/

gnc_numeric
gnc_numeric_add(gnc_numeric a, gnc_numeric b, 
                gint64 denom, gint how) {
  gnc_numeric sum;
  
  if(gnc_numeric_check(a) || gnc_numeric_check(b)) {
    return gnc_numeric_error(GNC_ERROR_ARG);
  }

  if((denom == GNC_DENOM_AUTO) && 
     (how & GNC_NUMERIC_DENOM_MASK) == GNC_DENOM_FIXED) {
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
  
  if(a.denom < 0) {
    a.num *= a.denom;
    a.denom = 1;
  }

  if(b.denom < 0) {
    b.num *= b.denom;
    b.denom = 1;
  }

  /* get an exact answer.. same denominator is the common case. */ 
  if(a.denom == b.denom) {
    sum.num = a.num + b.num;
    sum.denom = a.denom;
  }
  else {
    sum.num   = a.num*b.denom + b.num*a.denom;
    sum.denom = a.denom*b.denom;
  }
  
  if((denom == GNC_DENOM_AUTO) &&
     ((how & GNC_NUMERIC_DENOM_MASK) == GNC_DENOM_LCD)) {
    denom = gnc_numeric_lcd(a, b);
    how   = how & GNC_NUMERIC_RND_MASK;
  }
  
  return gnc_numeric_convert(sum, denom, how);                             
}


/********************************************************************
 *  gnc_numeric_add_fixed
 ********************************************************************/

gnc_numeric
gnc_numeric_add_fixed(gnc_numeric a, gnc_numeric b) {
  return gnc_numeric_add(a, b, GNC_DENOM_AUTO, 
                         GNC_DENOM_FIXED | GNC_RND_NEVER);
}


/********************************************************************
 *  gnc_numeric_sub
 ********************************************************************/

gnc_numeric
gnc_numeric_sub(gnc_numeric a, gnc_numeric b, 
                gint64 denom, gint how) {
  gnc_numeric diff;

  if(gnc_numeric_check(a) || gnc_numeric_check(b)) {
    return gnc_numeric_error(GNC_ERROR_ARG);
  }

  if((denom == GNC_DENOM_AUTO) && 
     (how & GNC_NUMERIC_DENOM_MASK) == GNC_DENOM_FIXED) {
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

  if(a.denom < 0) {
    a.num *= a.denom;
    a.denom = 1;
  }

  if(b.denom < 0) {
    b.num *= b.denom;
    b.denom = 1;
  }

  /* get an exact answer.. same denominator is the common case. */ 
  if(a.denom == b.denom) {
    diff.num = a.num - b.num;
    diff.denom = a.denom;
  }
  else {
    diff.num   = a.num*b.denom - b.num*a.denom;
    diff.denom = a.denom*b.denom;
  }
  
  if((denom == GNC_DENOM_AUTO) &&
     ((how & GNC_NUMERIC_DENOM_MASK) == GNC_DENOM_LCD)) {
    denom = gnc_numeric_lcd(a, b);
    how   = how & GNC_NUMERIC_RND_MASK;
  }
  return gnc_numeric_convert(diff, denom, how);                             
}


/********************************************************************
 *  gnc_numeric_sub_fixed
 ********************************************************************/

gnc_numeric
gnc_numeric_sub_fixed(gnc_numeric a, gnc_numeric b) {
  return gnc_numeric_sub(a, b, GNC_DENOM_AUTO, 
                         GNC_DENOM_FIXED | GNC_RND_NEVER);
}


/********************************************************************
 *  gnc_numeric_mul
 ********************************************************************/

gnc_numeric
gnc_numeric_mul(gnc_numeric a, gnc_numeric b, 
                gint64 denom, gint how) {
  gnc_numeric product;
  
  if(gnc_numeric_check(a) || gnc_numeric_check(b)) {
    return gnc_numeric_error(GNC_ERROR_ARG);
  }

  if((denom == GNC_DENOM_AUTO) && 
     (how & GNC_NUMERIC_DENOM_MASK) == GNC_DENOM_FIXED) {
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

  if(a.denom < 0) {
    a.num *= a.denom;
    a.denom = 1;
  }

  if(b.denom < 0) {
    b.num *= b.denom;
    b.denom = 1;
  }

  product.num   = a.num*b.num;
  product.denom = a.denom*b.denom;
  
  if(product.denom < 0) {
    product.num   = -product.num;
    product.denom = -product.denom;
  }
  
  if((denom == GNC_DENOM_AUTO) &&
     ((how & GNC_NUMERIC_DENOM_MASK) == GNC_DENOM_LCD)) {
    denom = gnc_numeric_lcd(a, b);
    how   = how & GNC_NUMERIC_RND_MASK;
  }

  return gnc_numeric_convert(product, denom, how);                             
}


/********************************************************************
 *  gnc_numeric_div
 ********************************************************************/

gnc_numeric
gnc_numeric_div(gnc_numeric a, gnc_numeric b, 
                gint64 denom, gint how) {
  gnc_numeric quotient;

  if(gnc_numeric_check(a) || gnc_numeric_check(b)) {
    return gnc_numeric_error(GNC_ERROR_ARG);
  }

  if((denom == GNC_DENOM_AUTO) && 
     (how & GNC_NUMERIC_DENOM_MASK) == GNC_DENOM_FIXED) {
    if(a.denom == b.denom) {
      denom = a.denom;
    }
    else if(a.denom == 0) {
      denom = b.denom;
    }
    else {
      return gnc_numeric_error(GNC_ERROR_DENOM_DIFF);
    }
  }
  

  if(a.denom < 0) {
    a.num *= a.denom;
    a.denom = 1;
  }

  if(b.denom < 0) {
    b.num *= b.denom;
    b.denom = 1;
  }

  if(a.denom == b.denom) {
    quotient.num = a.num;
    quotient.denom = b.num;
  }
  else {
    quotient.num   = a.num*b.denom;
    quotient.denom = a.denom*b.num;
  }
  
  if(quotient.denom < 0) {
    quotient.num   = -quotient.num;
    quotient.denom = -quotient.denom;
  }
  
  if((denom == GNC_DENOM_AUTO) &&
     ((how & GNC_NUMERIC_DENOM_MASK) == GNC_DENOM_LCD)) {
    denom = gnc_numeric_lcd(a, b);
    how   = how & GNC_NUMERIC_RND_MASK;
  }

  return gnc_numeric_convert(quotient, denom, how); 
}
 
/********************************************************************
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

/********************************************************************
 *  gnc_numeric_neg
 *  return the absolute value of the argument 
 ********************************************************************/

gnc_numeric
gnc_numeric_abs(gnc_numeric a) {
  if(gnc_numeric_check(a)) {
    return gnc_numeric_error(GNC_ERROR_ARG);
  }
  return gnc_numeric_create(ABS(a.num), a.denom);
}

/********************************************************************
 *  gnc_numeric_convert
 ********************************************************************/

gnc_numeric
gnc_numeric_convert(gnc_numeric in, gint64 denom, gint how) {
  gnc_numeric out;
  gnc_numeric temp;
  gint64      temp_bc;
  gint64      temp_a;
  gint64      remainder;  
  gint64      sign;
  gint        denom_neg=0;
  double      ratio, logratio;
  double      sigfigs;

  if(gnc_numeric_check(in)) {
    return gnc_numeric_error(GNC_ERROR_ARG);
  }
  
  if(denom == GNC_DENOM_AUTO) {
    switch(how & GNC_NUMERIC_DENOM_MASK) {
    case GNC_DENOM_EXACT:
      return in;
      break;
      
    case GNC_DENOM_REDUCE:
      /* reduce the input to a relatively-prime fraction */
      return gnc_numeric_reduce(in);
      break;
      
    case GNC_DENOM_FIXED:
      if(in.denom != denom) {
        return gnc_numeric_error(GNC_ERROR_DENOM_DIFF);
      }
      else {
        return in;
      }
      break;
      
    case GNC_DENOM_SIGFIG:
      ratio    = fabs(gnc_numeric_to_double(in));
      if(ratio < 10e-20) {
        logratio = 0;
      }
      else {
        logratio = log10(ratio);
        logratio = ((logratio > 0.0) ? 
                    (floor(logratio)+1.0) : (ceil(logratio)));
      }
      sigfigs  = GNC_NUMERIC_GET_SIGFIGS(how);

      if(sigfigs-logratio >= 0) {
        denom    = (gint64)(pow(10, sigfigs-logratio));
      }
      else {
        denom    = -((gint64)(pow(10, logratio-sigfigs)));
      }
      
      how = how & ~GNC_DENOM_SIGFIG & ~GNC_NUMERIC_SIGFIGS_MASK;
      break;

    case GNC_DENOM_LCD:
      /* this is a no-op. */
    default:
      break;
    }
  }
  
  /* make sure we need to do the work */
  if(in.denom == denom) {
    return in;
  }
  
  /* if the denominator of the input value is negative, get rid of that. */
  if(in.denom < 0) {
    in.num = in.num * (- in.denom);
    in.denom = 1;
  }
  
  sign = (in.num < 0) ? -1 : 1;

  /* if the denominator is less than zero, we are to interpret it as 
   * the reciprocal of its magnitude. */
  if(denom < 0) {
    denom     = - denom;
    denom_neg = 1;
    temp_a    = (in.num < 0) ? -in.num : in.num;
    temp_bc   = in.denom * denom;
    remainder = in.num % temp_bc;
    out.num   = in.num / temp_bc;
    out.denom = - denom;    
  }
  else {
    /* do all the modulo and int division on positive values to make
     * things a little clearer. Reduce the fraction denom/in.denom to
     * help with range errors (FIXME : need bigger intermediate rep) */
    temp.num   = denom;
    temp.denom = in.denom;
    temp       = gnc_numeric_reduce(temp);
  
    out.num   = in.num * temp.num;
    out.num   = (out.num < 0) ? -out.num : out.num;
    remainder = out.num % temp.denom;
    out.num   = out.num / temp.denom;
    out.denom = denom;
  }

  if(remainder > 0) {
    switch(how) {
    case GNC_RND_FLOOR:
      if(sign < 0) {
        out.num = out.num + 1;
      }
      break;
      
    case GNC_RND_CEIL:
      if(sign > 0) {
        out.num = out.num + 1;
      }
      break;
      
    case GNC_RND_TRUNC:
      break;

    case GNC_RND_PROMOTE:
      out.num = out.num + 1;
      break;
      
    case GNC_RND_ROUND_HALF_DOWN:
      if(denom_neg) {
        if((2 * remainder) > in.denom*denom) {
          out.num = out.num + 1;
        }
      }
      else if((2 * remainder) > temp.denom) {
        out.num = out.num + 1;
      }
      break;
      
    case GNC_RND_ROUND_HALF_UP:
      if(denom_neg) {
        if((2 * remainder) >= in.denom*denom) {
          out.num = out.num + 1;
        }
      }
      else if((2 * remainder ) >= temp.denom) {
        out.num = out.num + 1;
      }
      break;
      
    case GNC_RND_ROUND:
      if(denom_neg) {
        if((2 * remainder) > in.denom*denom) {
          out.num = out.num + 1;
        }
        else if((2 * remainder) == in.denom*denom) {
          if(out.num % 2) {
            out.num = out.num + 1;
          }
        }        
      }
      else {
        if((2 * remainder ) > temp.denom) {
          out.num = out.num + 1;
        }
        else if((2 * remainder) == temp.denom) {
          if(out.num % 2) {
            out.num = out.num + 1;
          }
        }    
      }    
      break;
      
    case GNC_RND_NEVER:
      return gnc_numeric_error(GNC_ERROR_REMAINDER);
      break;
    }
  }
  
  out.num = (sign > 0) ? out.num : (-out.num);
  
  return out;
}


/********************************************************************
 *  gnc_numeric_lcd
 *  Find the least common multiple of the denominators of 
 *  a and b
 ********************************************************************/

gint64
gnc_numeric_lcd(gnc_numeric a, gnc_numeric b) {
  gint64 current_divisor = 2;
  gint64 max_square;
  gint64 three_count = 0;
  gint64 small_denom;
  gint64 big_denom;

  if(gnc_numeric_check(a) || gnc_numeric_check(b)) {
    return GNC_ERROR_ARG;
  }
  
  if(b.denom < a.denom) {
    small_denom = b.denom;
    big_denom = a.denom;
  }
  else {
    small_denom = a.denom;
    big_denom = b.denom;
  }

  /* special case: smaller divides smoothly into larger */
  if((big_denom % small_denom) == 0) {
    return big_denom;
  }
  
  max_square = small_denom;
  
  /* the LCM algorithm : take the union of the prime factors of the
   * two args and multiply them together. To do this, we find the
   * successive prime factors of the smaller denominator and eliminate
   * them from the larger denominator, then multiply the smaller by
   * the remains of the larger. */
  while(current_divisor * current_divisor <= max_square) {
    if(((small_denom % current_divisor) == 0) &&
       ((big_denom % current_divisor) == 0)) {
      big_denom = big_denom / current_divisor;
    }
    else {
      if(current_divisor == 2) {
        current_divisor++;
      }
      else if(three_count == 3) { 
        current_divisor += 4;
        three_count = 1;
      }
      else {
        current_divisor += 2;
        three_count++;
      }
    }
    
    if((current_divisor > small_denom) ||
       (current_divisor > big_denom)) {
      break;
    }
  }
  
  return small_denom * big_denom;

}
  

/********************************************************************
 *  gnc_numeric_reduce
 *  reduce a fraction by GCF elimination.  This is NOT done as a
 *  part of the arithmetic API unless GNC_DENOM_REDUCE is specified 
 *  as the output denominator.
 ********************************************************************/

gnc_numeric
gnc_numeric_reduce(gnc_numeric in) {

  gint64   current_divisor = 2;
  gint64   max_square;
  gint64   num = (in.num < 0) ? (- in.num) : in.num ;
  gint64   denom = in.denom;
  int      three_count = 0;
  gnc_numeric out;

  if(gnc_numeric_check(in)) {
    return gnc_numeric_error(GNC_ERROR_ARG);
  }

  /* the strategy is to eliminate common factors from 
   * 2 up to 'max', where max is the smaller of the smaller
   * part of the fraction and the sqrt of the larger part of 
   * the fraction.  There's also the special case of the 
   * smaller of fraction parts being a common factor.
   * 
   * we test for 2 and 3 first, and thereafter skip all even numbers
   * and all odd multiples of 3 (that's every third odd number,
   * i.e. 9, 15, 21), thus the three_count stuff. */

  /* special case: one side divides evenly by the other */
  if (num == 0) {
    denom = 1;
  }
  else if((num > denom) && (num % denom == 0)) {
    num = num / denom;
    denom = 1;
  }
  else if ((num <= denom) && (denom % num == 0)) {
    denom = denom / num;
    num = 1;
  }

  max_square = (num > denom) ? denom : num;

  /* normal case: test 2, then 3, 5, 7, 11, etc.
   * (skip multiples of 2 and 3) */
  while(current_divisor * current_divisor <= max_square) {
    if((num % current_divisor == 0) &&
       (denom % current_divisor == 0)) {
      num = num / current_divisor;
      denom = denom / current_divisor;
    }
    else {
      if(current_divisor == 2) {
        current_divisor++;
      }
      else if(three_count == 3) { 
        current_divisor += 4;
        three_count = 1;
      }
      else {
        current_divisor += 2;
        three_count++;
      }
    }

    if((current_divisor > num) ||
       (current_divisor > denom)) {
      break;
    }
  }

  /* all calculations are done on positive num, since it's not 
   * well defined what % does for negative values */
  out.num   = (in.num < 0) ? (- num) : num;
  out.denom = denom;
  return out;
}

/********************************************************************
 *  double_to_gnc_numeric
 ********************************************************************/

gnc_numeric
double_to_gnc_numeric(double in, gint64 denom, gint how) {
  gnc_numeric out;
  gint64 int_part=0;
  double frac_part;
  gint64 frac_int=0;
  double logval; 
  double sigfigs;

  if((denom == GNC_DENOM_AUTO) && (how & GNC_DENOM_SIGFIG)) {
    if(fabs(in) < 10e-20) {
      logval = 0;
    }
    else {
      logval   = log10(fabs(in));
      logval   = ((logval > 0.0) ? 
                  (floor(logval)+1.0) : (ceil(logval)));
    }
    sigfigs  = GNC_NUMERIC_GET_SIGFIGS(how);
    if(sigfigs-logval >= 0) {
      denom    = (gint64)(pow(10, sigfigs-logval));
    }
    else {
      denom    = -((gint64)(pow(10, logval-sigfigs)));
    }

    how =  how & ~GNC_DENOM_SIGFIG & ~GNC_NUMERIC_SIGFIGS_MASK;
  }

  int_part  = (gint64)(floor(fabs(in)));
  frac_part = in - (double)int_part;
  
  int_part = int_part * denom;
  frac_part = frac_part * (double)denom;

  switch(how & GNC_NUMERIC_RND_MASK) {
  case GNC_RND_FLOOR:
    frac_int = (gint64)floor(frac_part);
    break;

  case GNC_RND_CEIL:
    frac_int = (gint64)ceil(frac_part);
    break;

  case GNC_RND_TRUNC:
    frac_int = (gint64)frac_part;
    break;
    
  case GNC_RND_ROUND:
  case GNC_RND_ROUND_HALF_UP:
    frac_int = (gint64)rint(frac_part);
    break;

  case GNC_RND_NEVER:
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

/********************************************************************
 *  gnc_numeric_to_double
 ********************************************************************/

double
gnc_numeric_to_double(gnc_numeric in) {
  if(in.denom >= 0) {
    return (double)in.num/(double)in.denom;
  }
  else {
    return (double)(in.num * in.denom);
  }
}


/********************************************************************
 *  gnc_numeric_create
 ********************************************************************/

gnc_numeric
gnc_numeric_create(gint64 num, gint64 denom) {
  gnc_numeric out;
  out.num = num;
  out.denom = denom;
  return out;
}


/********************************************************************
 *  gnc_numeric_error
 ********************************************************************/

gnc_numeric
gnc_numeric_error(int error_code) {
  if(abs(error_code) < 5) {
    //    PERR("%s", _numeric_error_strings[ - error_code]);
  }
  return gnc_numeric_create(error_code, 0LL);
}


/********************************************************************
 *  gnc_numeric_zero
 ********************************************************************/

gnc_numeric
gnc_numeric_zero(void) {
  return gnc_numeric_create(0LL, 1LL);
}


/********************************************************************
 *  gnc_numeric_num
 ********************************************************************/

gint64
gnc_numeric_num(gnc_numeric a) {
  return a.num;
}


/********************************************************************
 *  gnc_numeric_denom
 ********************************************************************/

gint64
gnc_numeric_denom(gnc_numeric a) {
  return a.denom;
}


/********************************************************************
 *  gnc_numeric_add_with_error
 ********************************************************************/

gnc_numeric
gnc_numeric_add_with_error(gnc_numeric a, gnc_numeric b, 
                           gint64 denom, gint how,
                           gnc_numeric * error) {

  gnc_numeric sum   = gnc_numeric_add(a, b, denom, how);
  gnc_numeric exact = gnc_numeric_add(a, b, GNC_DENOM_AUTO, 
                                      GNC_DENOM_REDUCE);
  gnc_numeric err   = gnc_numeric_sub(sum, exact, GNC_DENOM_AUTO,
                                      GNC_DENOM_REDUCE);

  if(error) {
    *error = err;
  }
  return sum;
}

/********************************************************************
 *  gnc_numeric_sub_with_error
 ********************************************************************/

gnc_numeric
gnc_numeric_sub_with_error(gnc_numeric a, gnc_numeric b, 
                           gint64 denom, gint how,
                           gnc_numeric * error) {

  gnc_numeric diff  = gnc_numeric_sub(a, b, denom, how);
  gnc_numeric exact = gnc_numeric_sub(a, b, GNC_DENOM_AUTO,
                                      GNC_DENOM_REDUCE);
  gnc_numeric err   = gnc_numeric_sub(diff, exact, GNC_DENOM_AUTO, 
                                      GNC_DENOM_REDUCE);
  if(error) {
    *error = err;
  }
  return diff;
}


/********************************************************************
 *  gnc_numeric_mul_with_error
 ********************************************************************/

gnc_numeric
gnc_numeric_mul_with_error(gnc_numeric a, gnc_numeric b, 
                           gint64 denom, gint how,
                           gnc_numeric * error) {

  gnc_numeric prod  = gnc_numeric_mul(a, b, denom, how);
  gnc_numeric exact = gnc_numeric_mul(a, b, GNC_DENOM_AUTO,
                                      GNC_DENOM_REDUCE);
  gnc_numeric err   = gnc_numeric_sub(prod, exact, GNC_DENOM_AUTO,
                                      GNC_DENOM_REDUCE);
  if(error) {
    *error = err;
  }
  return prod;
}


/********************************************************************
 *  gnc_numeric_div_with_error
 ********************************************************************/

gnc_numeric
gnc_numeric_div_with_error(gnc_numeric a, gnc_numeric b, 
                           gint64 denom, gint how,
                           gnc_numeric * error) {

  gnc_numeric quot  = gnc_numeric_div(a, b, denom, how);
  gnc_numeric exact = gnc_numeric_div(a, b, GNC_DENOM_AUTO, 
                                      GNC_DENOM_REDUCE);
  gnc_numeric err   = gnc_numeric_sub(quot, exact, 
                                      GNC_DENOM_AUTO, GNC_DENOM_REDUCE);
  if(error) {
    *error = err;
  }
  return quot;
}

int
gnc_numeric_check(gnc_numeric in) {
  if(in.denom != 0) {
    return GNC_ERROR_OK;
  }
  else if(in.num) {
    return in.num;
  }
  else {
    return GNC_ERROR_ARG;
  }
}

/********************************************************************
 *  gnc_numeric text IO
 ********************************************************************/

gchar *
gnc_numeric_to_string(gnc_numeric n) {
  gchar *result;
  long long tmpnum = n.num;
  long long tmpdenom = n.denom;
  result = g_strdup_printf("%lld/%lld", tmpnum, tmpdenom);
  return result;
}

const gchar *
string_to_gnc_numeric(const gchar* str, gnc_numeric *n) {
  /* Read a gnc_numeric from str, skipping any leading whitespace, and
     returning a pointer to just past the last byte read.  Return NULL
     on error. */
  int num_read;
  long long tmpnum;
  long long tmpdenom;
    
  if(!str) return NULL;

  /* must use "<" here because %n's effects aren't well defined */
  if(sscanf(str, " %lld/%lld%n", &tmpnum, &tmpdenom, &num_read) < 2) {
    return(NULL);
  }
  n->num = tmpnum;
  n->denom = tmpdenom;
  return(str + num_read);
}

#ifdef _GNC_NUMERIC_TEST

static char *
gnc_numeric_print(gnc_numeric in) {
  char * retval;
  if(gnc_numeric_check(in)) {
    retval = g_strdup_printf("<ERROR> [%lld / %lld]", in.num, in.denom); 
  }
  else {
    retval = g_strdup_printf("[%lld / %lld]", in.num, in.denom); 
  }
  return retval;
}

int
main(int argc, char ** argv) {
  gnc_numeric a = gnc_numeric_create(1, 3);
  gnc_numeric b = gnc_numeric_create(1, 4);
  gnc_numeric c;
  gnc_numeric d = gnc_numeric_create(1, 2);
  
  gnc_numeric err;
  int i;

  printf("add exact : %s + %s = %s\n",
         gnc_numeric_print(a), gnc_numeric_print(b),
         gnc_numeric_print(gnc_numeric_add(a, b, 
                                           GNC_DENOM_AUTO, 
                                           GNC_DENOM_EXACT)));
  
  
  printf("add least : %s + %s = %s\n",
         gnc_numeric_print(a), gnc_numeric_print(b),
         gnc_numeric_print(gnc_numeric_add(a, b, 
                                           GNC_DENOM_AUTO, 
                                           GNC_DENOM_REDUCE)));
  
  printf("add 100ths (banker's): %s + %s = %s\n",
         gnc_numeric_print(a), gnc_numeric_print(b),
         gnc_numeric_print(gnc_numeric_add(a, b, 100,
                                           GNC_RND_ROUND)));
  
  c = gnc_numeric_add_with_error(a, b, 100, GNC_RND_ROUND, &err);
  printf("add 100ths/error : %s + %s = %s + (error) %s\n\n",
         gnc_numeric_print(a), gnc_numeric_print(b),
         gnc_numeric_print(c),
         gnc_numeric_print(err));
  
  printf("sub exact : %s - %s = %s\n",
         gnc_numeric_print(a), gnc_numeric_print(b),
         gnc_numeric_print(gnc_numeric_sub(a, b, GNC_DENOM_AUTO, 
                                           GNC_DENOM_EXACT)));
  
  printf("sub least : %s - %s = %s\n",
         gnc_numeric_print(a), gnc_numeric_print(b),
         gnc_numeric_print(gnc_numeric_sub(a, b, 
                                           GNC_DENOM_AUTO, 
                                           GNC_DENOM_REDUCE)));
  
  printf("sub 100ths : %s - %s = %s\n",
         gnc_numeric_print(a), gnc_numeric_print(b),
         gnc_numeric_print(gnc_numeric_sub(a, b, 100,
                                           GNC_RND_ROUND)));
  
  c = gnc_numeric_sub_with_error(a, b, 100, GNC_RND_FLOOR, &err);
  printf("sub 100ths/error : %s - %s = %s + (error) %s\n\n",
         gnc_numeric_print(a), gnc_numeric_print(b),
         gnc_numeric_print(c),
         gnc_numeric_print(err));
  
  printf("mul exact : %s * %s = %s\n",
         gnc_numeric_print(a), gnc_numeric_print(b),
         gnc_numeric_print(gnc_numeric_mul(a, b, GNC_DENOM_AUTO, 
                                           GNC_DENOM_EXACT)));

  printf("mul least : %s * %s = %s\n",
         gnc_numeric_print(a), gnc_numeric_print(b),
         gnc_numeric_print(gnc_numeric_mul(a, b, GNC_DENOM_AUTO, 
                                           GNC_DENOM_REDUCE)));
  
  printf("mul 100ths : %s * %s = %s\n",
         gnc_numeric_print(a), gnc_numeric_print(b),
         gnc_numeric_print(gnc_numeric_mul(a, b, 100,
                                           GNC_RND_ROUND)));

  c = gnc_numeric_mul_with_error(a, b, 100, GNC_RND_ROUND, &err);
  printf("mul 100ths/error : %s * %s = %s + (error) %s\n\n",
         gnc_numeric_print(a), gnc_numeric_print(b),
         gnc_numeric_print(c),
         gnc_numeric_print(err));
  
  printf("div exact : %s / %s = %s\n",
         gnc_numeric_print(a), gnc_numeric_print(b),
         gnc_numeric_print(gnc_numeric_div(a, b, GNC_DENOM_AUTO, 
                                           GNC_DENOM_EXACT)));
  
  printf("div least : %s / %s = %s\n",
         gnc_numeric_print(a), gnc_numeric_print(b),
         gnc_numeric_print(gnc_numeric_div(a, b, GNC_DENOM_AUTO, 
                                           GNC_DENOM_REDUCE)));
  
  printf("div 100ths : %s / %s = %s\n",
         gnc_numeric_print(a), gnc_numeric_print(b),
         gnc_numeric_print(gnc_numeric_div(a, b, 100,
                                           GNC_RND_ROUND)));  
  
  c = gnc_numeric_div_with_error(a, b, 100, GNC_RND_ROUND, &err);
  printf("div 100ths/error : %s / %s = %s + (error) %s\n\n",
         gnc_numeric_print(a), gnc_numeric_print(b),
         gnc_numeric_print(c),
         gnc_numeric_print(err));
  
  printf("7/16 as float: %e\n",
         gnc_numeric_to_double(gnc_numeric_create(7, 16)));
  
  printf("7/16 as 100ths (floor): %s\n",
         gnc_numeric_print(gnc_numeric_convert(gnc_numeric_create(7, 16),
                                               100, GNC_RND_FLOOR)));
  printf("7/16 as 100ths (ceil): %s\n",
         gnc_numeric_print(gnc_numeric_convert(gnc_numeric_create(7, 16),
                                               100, GNC_RND_CEIL)));
  printf("7/16 as 100ths (trunc): %s\n",
         gnc_numeric_print(gnc_numeric_convert(gnc_numeric_create(7, 16),
                                               100, GNC_RND_TRUNC)));
  printf("7/16 as 100ths (round): %s\n",
         gnc_numeric_print(gnc_numeric_convert(gnc_numeric_create(7, 16),
                                               100, GNC_RND_ROUND)));

  printf("1511/1000 as 1/100 (round): %s\n",
         gnc_numeric_print(gnc_numeric_convert(gnc_numeric_create(1511, 1000),
                                               100, GNC_RND_ROUND)));
  printf("1516/1000 as 1/100 (round): %s\n",
         gnc_numeric_print(gnc_numeric_convert(gnc_numeric_create(1516, 1000),
                                               100, GNC_RND_ROUND)));
  printf("1515/1000 as 1/100 (round): %s\n",
         gnc_numeric_print(gnc_numeric_convert(gnc_numeric_create(1515, 1000),
                                               100, GNC_RND_ROUND)));
  printf("1525/1000 as 1/100 (round): %s\n",
         gnc_numeric_print(gnc_numeric_convert(gnc_numeric_create(1525, 1000),
                                               100, GNC_RND_ROUND)));

  printf("100023234 / 334216654 reduced: %s\n",
         gnc_numeric_print(gnc_numeric_reduce(gnc_numeric_create(10023234LL,
                                                                 334216654LL))));
  printf("2^10*3^10*17^2 / 2^8*3^12 reduced: %s\n",
         gnc_numeric_print
         (gnc_numeric_reduce(gnc_numeric_create(17474724864LL,
                                                136048896LL))));
  printf("1024 / 1024^4 reduced: %s\n",
         gnc_numeric_print
         (gnc_numeric_reduce(gnc_numeric_create(1024LL,
                                                1099511627776LL))));
  printf("reducing 100,000 times:\n\n");
  for(i = 0; i < 100000; i++) {
    gnc_numeric_reduce(gnc_numeric_create(17474724864LL,
                                          136048896LL));
  }
  
  printf("add LCM: %s + %s = %s\n",
         gnc_numeric_print(b), gnc_numeric_print(d),
         gnc_numeric_print(gnc_numeric_add(b, d, GNC_DENOM_AUTO,
                                           GNC_DENOM_LCD)));
 
  printf("float to 6 sigfigs: %s\n",
         gnc_numeric_print(double_to_gnc_numeric(1.1234567890123, 
                                                 GNC_DENOM_AUTO, 
                                                 GNC_DENOM_SIGFIGS(6) |
                                                 GNC_RND_ROUND)));
  printf("float to 6 sigfigs: %s\n",
         gnc_numeric_print(double_to_gnc_numeric(.011234567890123, 
                                                 GNC_DENOM_AUTO, 
                                                 GNC_DENOM_SIGFIGS(6) |
                                                 GNC_RND_ROUND)));
  printf("float to 6 sigfigs: %s\n",
         gnc_numeric_print(double_to_gnc_numeric(1123.4567890123, 
                                                 GNC_DENOM_AUTO, 
                                                 GNC_DENOM_SIGFIGS(6) |
                                                 GNC_RND_ROUND)));
  printf("float to 6 sigfigs: %s\n",
         gnc_numeric_print(double_to_gnc_numeric(1.1234567890123e-5, 
                                                 GNC_DENOM_AUTO, 
                                                 GNC_DENOM_SIGFIGS(6) |
                                                 GNC_RND_ROUND)));
  printf("add to 4 sigfigs: %s + %s = %s\n",
         gnc_numeric_print(a), gnc_numeric_print(b),
         gnc_numeric_print(gnc_numeric_add(a, b, 
                                           GNC_DENOM_AUTO, 
                                           GNC_DENOM_SIGFIGS(4) |
                                           GNC_RND_ROUND)));
  
   
  return 0;
}
#endif
