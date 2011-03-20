/********************************************************************\
 * pow.h -- pow wrapper for MinGW systems                           *
 * Copyright (C) 2007 Andreas Koehler <andi5.py@gmx.net>            *
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
\********************************************************************/

#ifndef __POW_H__
#define __POW_H__

#include <math.h>

/* From MinGW, math.h: */

/* Excess precision when using a 64-bit mantissa for FPU math ops can
 * cause unexpected results with some of the MSVCRT math functions.
 * For example, unless the function return value is stored (truncating
 * to 53-bit mantissa), calls to pow with both x and y as integral
 * values sometimes produce a non-integral result. */

#define __DEFINE_FLOAT_STORE_MATHFN_D1(fn1)	\
static __inline__ double			\
__float_store_ ## fn1 (double x)		\
{						\
   __volatile__ double res = (fn1) (x);		\
  return res;					\
}

#define __DEFINE_FLOAT_STORE_MATHFN_D2(fn2)	\
static __inline__ double			\
__float_store_ ## fn2 (double x, double y)	\
{						\
  __volatile__ double res = (fn2) (x, y);	\
  return res;					\
}

#undef pow

/* Define the ___float_store_pow function and use it instead of pow(). */
__DEFINE_FLOAT_STORE_MATHFN_D2 (pow)
#define pow __float_store_pow

#endif /* __POW_H__ */
