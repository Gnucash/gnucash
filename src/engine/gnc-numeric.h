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

#ifndef __GNC_NUMERIC_H__
#define __GNC_NUMERIC_H__

struct _gnc_numeric {
  gint64  num;
  gint64  denom;
};

typedef struct _gnc_numeric gnc_numeric;

typedef enum { 
  GNC_RND_FLOOR, GNC_RND_CEIL, GNC_RND_TRUNC,
  GNC_RND_ROUND, GNC_RND_ROUND_HALF_UP, 
  GNC_RND_NEVER
} gnc_numeric_round_t;

#define GNC_DENOM_EXACT  -1
#define GNC_DENOM_REDUCE -2 
#define GNC_DENOM_LCD    -3
#define GNC_DENOM_FIXED  -4

#define GNC_ERROR_OK            0
#define GNC_ERROR_ARG          -1  /* argument has negative/0 denominator */
#define GNC_ERROR_OVERFLOW     -2  /* operation would overflow */
#define GNC_ERROR_DENOM_DIFF   -3  /* FIXED specified but denoms differ */

/* the primitive error macro */
#define GNC_NUMERIC_ERROR( err ) return gnc_numeric_create(0LL, err) 

/* make a gnc_numeric from numerator and denominator */
gnc_numeric gnc_numeric_create(gint64 num, gint64 denom);

/* arithmetic operations */
gnc_numeric gnc_numeric_add(gnc_numeric a, gnc_numeric b, 
                            gint64 denom, gnc_numeric_round_t how);
gnc_numeric gnc_numeric_sub(gnc_numeric a, gnc_numeric b, 
                            gint64 denom, gnc_numeric_round_t how);
gnc_numeric gnc_numeric_mul(gnc_numeric a, gnc_numeric b, 
                            gint64 denom, gnc_numeric_round_t how);
gnc_numeric gnc_numeric_div(gnc_numeric a, gnc_numeric b, 
                            gint64 denom, gnc_numeric_round_t how);

/* arithmetic functions with exact error returns */
gnc_numeric gnc_numeric_add_with_error(gnc_numeric a, gnc_numeric b,
                                       gint64 denom, gnc_numeric_round_t how,
                                       gnc_numeric * error);
gnc_numeric gnc_numeric_sub_with_error(gnc_numeric a, gnc_numeric b,
                                       gint64 denom, gnc_numeric_round_t how,
                                       gnc_numeric * error);
gnc_numeric gnc_numeric_mul_with_error(gnc_numeric a, gnc_numeric b,
                                       gint64 denom, gnc_numeric_round_t how,
                                       gnc_numeric * error);
gnc_numeric gnc_numeric_div_with_error(gnc_numeric a, gnc_numeric b,
                                       gint64 denom, gnc_numeric_round_t how,
                                       gnc_numeric * error);

/* change the denominator of a gnc_numeric value */
gnc_numeric gnc_numeric_convert(gnc_numeric in, gint64 denom, 
                                gnc_numeric_round_t how);

gnc_numeric gnc_numeric_convert_with_error(gnc_numeric in, gint64 denom, 
                                           gnc_numeric_round_t how,
                                           gnc_numeric * error);

/* check for error signal in value */ 
int         gnc_numeric_check(gnc_numeric a);

/* reduce by GCF elimination */
gnc_numeric gnc_numeric_reduce(gnc_numeric in);

/* convert to and from floating-point values */
gnc_numeric double_to_gnc_numeric(double in, gint64 denom,  
                                  gnc_numeric_round_t how);
double      gnc_numeric_to_double(gnc_numeric in);

#endif
