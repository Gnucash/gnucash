/*******************************************************************\
 * EuroUtils.c -- utilities for EURO currency                       *
 *                                                                  *
 * Copyright (C) 2000 Herbert Thoma                                 *
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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
 *                                                                  *
\********************************************************************/

#include <math.h>
#include <stdlib.h>
#include <string.h>

/* local structs */

typedef struct _gnc_euro_rate_struct
{
  const char *currency;
  double rate;
}
gnc_euro_rate_struct;

/* This array MUST be sorted ! */
static gnc_euro_rate_struct _gnc_euro_rate_[] =
{
  { "BFR",  40.3399 },  /* belgian franc */
  { "DEM",  1.95583 },  /* german mark */
  { "DM",   1.95583 },  /* german mark */
  { "ESC",  200.482 },  /* portugese escudo */
  { "EUR",  1.00000 },  /* euro */
  { "EURO", 1.00000 },  /* euro */
  { "FF",   6.55957 },  /* french franc */
  { "FMK",  5.94573 },  /* finnmark */
  { "HFL",  2.20371 },  /* netherland gulden */
  { "IRP",  .787564 },  /* irish pound */
  { "LFR",  40.3399 },  /* luxembourg franc */
  { "LIT",  1936.27 },  /* italian lira */
  { "PTA",  166.386 },  /* spanish peseta */
  { "S",    13.7603 }   /* austrian schilling */
};

static int _gnc_euro_rate_compare_(const void *euro_rate_struct1, const void *euro_rate_struct2)
{
  return (strcmp(((const gnc_euro_rate_struct *)euro_rate_struct1)->currency,
		 ((const gnc_euro_rate_struct *)euro_rate_struct2)->currency));
}

/* ------------------------------------------------------ */

int gnc_is_euro_currency(const char *currency)
{
  gnc_euro_rate_struct test_currency;
  gnc_euro_rate_struct *result;

  test_currency.currency = currency;
  result = (gnc_euro_rate_struct *)bsearch(&test_currency, _gnc_euro_rate_,
					   sizeof(_gnc_euro_rate_)/sizeof(gnc_euro_rate_struct), sizeof(gnc_euro_rate_struct),
					   _gnc_euro_rate_compare_);

  if(result == NULL)
  {
    return 0;
  }
  else
  {
    return 1;
  }
}

/* ------------------------------------------------------ */

double gnc_convert_to_euro(const char *currency, double value)
{
  gnc_euro_rate_struct test_currency;
  gnc_euro_rate_struct *result;

  test_currency.currency = currency;
  result = (gnc_euro_rate_struct *)bsearch(&test_currency, _gnc_euro_rate_,
					   sizeof(_gnc_euro_rate_)/sizeof(gnc_euro_rate_struct), sizeof(gnc_euro_rate_struct),
					   _gnc_euro_rate_compare_);

  if(result == NULL)
  {
    return value;
  }
  else
  {
    return (floor(((value / result->rate) * 100.0) + 0.5) / 100.0); /* round to 2 decimal places */
  }
}

/* ------------------------------------------------------ */

double gnc_convert_from_euro(const char *currency, double value)
{
  gnc_euro_rate_struct test_currency;
  gnc_euro_rate_struct *result;

  test_currency.currency = currency;
  result = (gnc_euro_rate_struct *)bsearch(&test_currency, _gnc_euro_rate_,
					   sizeof(_gnc_euro_rate_)/sizeof(gnc_euro_rate_struct), sizeof(gnc_euro_rate_struct),
					   _gnc_euro_rate_compare_);

  if(result == NULL)
  {
    return value;
  }
  else
  {
    return (value * result->rate);
  }
}

/************************** END OF FILE *************************/
