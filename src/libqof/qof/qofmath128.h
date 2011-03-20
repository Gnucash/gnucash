/********************************************************************
 * qofmath128.h -- an 128-bit integer library                       *
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

#ifndef QOF_MATH_128_H
#define QOF_MATH_128_H

#include <glib.h>

/** @addtogroup Math128
 *  Quick-n-dirty 128-bit integer math lib.   Things seem to mostly
 *  work, and have been tested, but not comprehensively tested.
 * @{
 */

typedef struct
{
    guint64 hi;
    guint64 lo;
    short isneg;    /**< sign-bit -- T if number is negative */
    short isbig;    /**< sizeflag -- T if number won't fit in signed 64-bit */
} qofint128;

/** Return true of two numbers are equal */
gboolean equal128 (qofint128 a, qofint128 b);

/** Return returns 1 if a>b, -1 if b>a, 0 if a == b */
int cmp128 (qofint128 a, qofint128 b);

/** Shift right by one bit (i.e. divide by two) */
qofint128 shift128 (qofint128 x);

/** Shift left by one bit (i.e. multiply by two) */
qofint128 shiftleft128 (qofint128 x);

/** Increment by one */
qofint128 inc128 (qofint128 a);

/** Add a pair of 128-bit numbers, returning a 128-bit number */
qofint128 add128 (qofint128 a, qofint128 b);

/** Multiply a pair of signed 64-bit numbers,
 *  returning a signed 128-bit number.
 */
qofint128 mult128 (gint64 a, gint64 b);

/** Divide a signed 128-bit number by a signed 64-bit,
 *  returning a signed 128-bit number.
 */
qofint128 div128 (qofint128 n, gint64 d);

/** Return the remainder of a signed 128-bit number modulo
 *  a signed 64-bit.  That is, return n%d in 128-bit math.
 *  I beleive that ths algo is overflow-free, but should be
 *  audited some more ...
 */
gint64 rem128 (qofint128 n, gint64 d);

/** Return the greatest common factor of two 64-bit numbers */
guint64 gcf64(guint64 num, guint64 denom);

/** Return the least common multiple of two 64-bit numbers. */
qofint128 lcm128 (guint64 a, guint64 b);

#endif

/** @} */
