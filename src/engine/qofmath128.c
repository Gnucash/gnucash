/********************************************************************
 * qofmath128.c -- an 128-bit integer library                       *
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
 *******************************************************************/

#define _GNU_SOURCE

#include "config.h"
#include "qofmath128.h"

#include <glib.h>

/* =============================================================== */
/*
 *  Quick-n-dirty 128-bit integer math lib.   Things seem to mostly
 *  work, and have been tested, but not comprehensively tested.
 */

/** Multiply a pair of signed 64-bit numbers, 
 *  returning a signed 128-bit number.
 */
inline qofint128
mult128 (gint64 a, gint64 b)
{
  qofint128 prod;

  prod.isneg = 0;
  if (0>a)
  {
    prod.isneg = !prod.isneg;
    a = -a;
  }

  if (0>b)
  {
    prod.isneg = !prod.isneg;
    b = -b;
  }

  guint64 a1 = a >> 32;
  guint64 a0 = a - (a1<<32);

  guint64 b1 = b >> 32;
  guint64 b0 = b - (b1<<32);

  guint64 d = a0*b0;
  guint64 d1 = d >> 32;
  guint64 d0 = d - (d1<<32);

  guint64 e = a0*b1;
  guint64 e1 = e >> 32;
  guint64 e0 = e - (e1<<32);

  guint64 f = a1*b0;
  guint64 f1 = f >> 32;
  guint64 f0 = f - (f1<<32);

  guint64 g = a1*b1;
  guint64 g1 = g >> 32;
  guint64 g0 = g - (g1<<32);

  guint64 sum = d1+e0+f0;
  guint64 carry = 0;
  /* Can't say 1<<32 cause cpp will goof it up; 1ULL<<32 might work */
  guint64 roll = 1<<30;
  roll <<= 2;

  guint64 pmax = roll-1;
  while (pmax < sum)
  {
    sum -= roll;
    carry ++;
  }

  prod.lo = d0 + (sum<<32);
  prod.hi = carry + e1 + f1 + g0 + (g1<<32);
  // prod.isbig = (prod.hi || (sum >> 31));
  prod.isbig = prod.hi || (prod.lo >> 63);

  return prod;
}

/** Divide a signed 128-bit number by a signed 64-bit,
 *  returning a signed 128-bit number.
 */
inline qofint128
div128 (qofint128 n, gint64 d)
{
  qofint128 quotient;
  guint64 hirem;   /* hi remainder */
  guint64 qlo;

  quotient.isneg = n.isneg;
  if (0 > d)
  {
    d = -d;
    quotient.isneg = !quotient.isneg;
  }

  quotient.hi = n.hi / d;
  hirem = n.hi - quotient.hi * d;
  
  guint64 lo = 1<<30;
  lo <<= 33;
  lo /= d;
  lo <<= 1;

  lo *= hirem; 
  quotient.lo = lo + n.lo/d;

  /* Deal with low remainder bits.
   * Is there a more efficient way of doing this?
   */
  qofint128 mu = mult128 (quotient.lo, d);

  gint64 nn = 0x7fffffffffffffffULL & n.lo;
  gint64 rr = 0x7fffffffffffffffULL & mu.lo;
  gint64 rnd = nn - rr;
  rnd /= d;   

  /* ?? will this ever overflow ? */
  qlo = quotient.lo;
  quotient.lo += rnd;
  if (qlo > quotient.lo)
  {
    quotient.hi += 1;
  }

  /* compute the carry situation */
  quotient.isbig = (quotient.hi || (quotient.lo >> 63));

  return quotient;
}

/** Return the remainder of a signed 128-bit number modulo 
 *  a signed 64-bit.  That is, return n%d in 128-bit math.
 *  I beleive that ths algo is overflow-free, but should be 
 *  audited some more ... 
 */
inline gint64
rem128 (qofint128 n, gint64 d)
{
  qofint128 quotient = div128 (n,d);

  qofint128 mu = mult128 (quotient.lo, d);

  gint64 nn = 0x7fffffffffffffffULL & n.lo;
  gint64 rr = 0x7fffffffffffffffULL & mu.lo;
  return nn - rr;
}

/** Return the ratio n/d reduced so that there are no common factors. */
inline gnc_numeric
reduce128(qofint128 n, gint64 d)
{
  gint64   t;
  gint64   num;
  gint64   denom;
  gnc_numeric out;

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

  qofint128 red = div128 (n, num);
  if (red.isbig)
  {
    return gnc_numeric_error (GNC_ERROR_OVERFLOW);
  }
  out.num   = red.lo;
  if (red.isneg) out.num = -out.num;
  out.denom = d / num;
  return out;
}

/** Return true of two numbers are equal */
inline gboolean
equal128 (qofint128 a, qofint128 b)
{
	if (a.lo != b.lo) return 0;
	if (a.hi != b.hi) return 0;
	if (a.isneg != b.isneg) return 0;
	return 1;
}

/** Return the greatest common factor of two 64-bit numbers */
inline guint64
gcf64(guint64 num, guint64 denom)
{
  guint64   t;

  t =  num % denom;
  num = denom;
  denom = t;

  /* The strategy is to use Euclid's algorithm */
  while (0 != denom) 
  {
    t = num % denom;
    num = denom;
    denom = t;
  }
  /* num now holds the GCD (Greatest Common Divisor) */
  return num;
}

/** Return the least common multiple of two 64-bit numbers. */
inline qofint128
lcm128 (guint64 a, guint64 b)
{
  guint64 gcf = gcf64 (a,b);
  b /= gcf;
  return mult128 (a,b);
}

/** Add a pair of 128-bit numbers, returning a 128-bit number */
inline qofint128
add128 (qofint128 a, qofint128 b)
{
  qofint128 sum;
  if (a.isneg == b.isneg)
  {
    sum.isneg = a.isneg;
    sum.hi = a.hi + b.hi;
    sum.lo = a.lo + b.lo;
    if ((sum.lo < a.lo) || (sum.lo < b.lo))
    {
     sum.hi ++;
    }
    sum.isbig = sum.hi || (sum.lo >> 63);
    return sum;
  }
  if ((b.hi > a.hi) ||
     ((b.hi == a.hi) && (b.lo > a.lo)))
  {
    qofint128 tmp = a;
    a = b;
    b = tmp;
  }

  sum.isneg = a.isneg;
  sum.hi = a.hi - b.hi;
  sum.lo = a.lo - b.lo;

  if (sum.lo > a.lo)
  {
    sum.hi --;
  }

  sum.isbig = sum.hi || (sum.lo >> 63);
  return sum;
}

/** Shift right by one bit (i.e. divide by two) */
inline qofint128
shift128 (qofint128 x)
{
  guint64 sbit = x.hi & 0x1;
  x.hi >>= 1;
  x.lo >>= 1;
  x.isbig = 0;
  if (sbit)
  {
    sbit = 1<<30;  /* in two step to avoid 1ULL<<63 */
    sbit <<= 33;
    x.lo |= sbit;
    x.isbig = 1;
    return x;
  }
  if (x.hi)
  {
    x.isbig = 1;
  }
  return x;
}

#ifdef TEST_128_BIT_MULT
static void pr (gint64 a, gint64 b)
{
   qofint128 prod = mult128 (a,b);
   printf ("%lld * %lld = %lld %llu (0x%llx %llx) %hd\n",
	   a, b, prod.hi, prod.lo, prod.hi, prod.lo, prod.isbig);
}

static void prd (gint64 a, gint64 b, gint64 c)
{
   qofint128 prod = mult128 (a,b);
   qofint128 quot = div128 (prod, c);
   gint64 rem = rem128 (prod, c);
   printf ("%lld * %lld / %lld = %lld %llu + %lld (0x%llx %llx) %hd\n",
	   a, b, c, quot.hi, quot.lo, rem, quot.hi, quot.lo, quot.isbig);
}

int main ()
{
  pr (2,2);

  gint64 x = 1<<30;
  x <<= 2;

  pr (x,x);
  pr (x+1,x);
  pr (x+1,x+1);

  pr (x,-x);
  pr (-x,-x);
  pr (x-1,x);
  pr (x-1,x-1);
  pr (x-2,x-2);

  x <<= 1;
  pr (x,x);
  pr (x,-x);

  pr (1000000, 10000000000000);

  prd (x,x,2);
  prd (x,x,3);
  prd (x,x,4);
  prd (x,x,5);
  prd (x,x,6);

  x <<= 29;
  prd (3,x,3);
  prd (6,x,3);
  prd (99,x,3);
  prd (100,x,5);
  prd (540,x,5);
  prd (777,x,7);
  prd (1111,x,11);

  return 0;
}

#endif /* TEST_128_BIT_MULT */

/* ======================== END OF FILE =================== */
