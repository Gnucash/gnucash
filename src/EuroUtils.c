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

#include "gnc-commodity.h"
#include "gnc-engine.h"

#include "EuroUtils.h"

/* local structs */
typedef struct _gnc_euro_rate_struct {
  const char *currency;
  double rate;
} gnc_euro_rate_struct;


/* This array MUST be sorted ! */
static gnc_euro_rate_struct _gnc_euro_rate_[] =
{
  { "ATS",  13.7603 },  /* austrian schilling */
  { "BEF",  40.3399 },  /* belgian franc */
  { "BFR",  40.3399 },  /* belgian franc */
  { "DEM",  1.95583 },  /* german mark */
  { "DM",   1.95583 },  /* german mark */
  { "ESC",  200.482 },  /* portugese escudo */
  { "ESP",  166.386 },  /* spanish peseta */
  { "EUR",  1.00000 },  /* euro */
  { "EURO", 1.00000 },  /* euro */
  { "FF",   6.55957 },  /* french franc */
  { "FIM",  5.94573 },  /* finnmark */
  { "FMK",  5.94573 },  /* finnmark */
  { "FRF",  6.55957 },  /* french franc */
  { "GRD",  340.750 },  /* greek drachma */
  { "HFL",  2.20371 },  /* netherland gulden */
  { "IEP",  .787564 },  /* irish pound */
  { "IRP",  .787564 },  /* irish pound */
  { "ITL",  1936.27 },  /* italian lira */
  { "LFR",  40.3399 },  /* luxembourg franc */
  { "LIT",  1936.27 },  /* italian lira */
  { "LUF",  40.3399 },  /* luxembourg franc */
  { "NLG",  2.20371 },  /* netherland gulden */
  { "PTA",  166.386 },  /* spanish peseta */
  { "PTE",  200.482 },  /* portugese escudo */
  { "S",    13.7603 },  /* austrian schilling */
  { "SCH",  13.7603 }   /* austrian schilling */
};

static int 
_gnc_euro_rate_compare_(const void * key, 
                        const void * value)
{
  const gnc_commodity * curr = key;
  const gnc_euro_rate_struct * euro = value;

  if (!key || !value)
    return -1;

  return strcasecmp(gnc_commodity_get_mnemonic(curr), euro->currency);

}

/* ------------------------------------------------------ */

gboolean
gnc_is_euro_currency(const gnc_commodity * currency) {

  gnc_euro_rate_struct *result;
  const char *namespace;

  if (currency == NULL)
    return FALSE;

  namespace = gnc_commodity_get_namespace (currency);
  if (namespace == NULL)
    return FALSE;

  if (strcmp (namespace, GNC_COMMODITY_NS_ISO) != 0)
    return FALSE;

  result = bsearch(currency,
                   _gnc_euro_rate_,
                   sizeof(_gnc_euro_rate_) / sizeof(gnc_euro_rate_struct), 
                   sizeof(gnc_euro_rate_struct),
                   _gnc_euro_rate_compare_);

  if (result == NULL)
    return FALSE;

  return TRUE;
}

/* ------------------------------------------------------ */

gnc_numeric
gnc_convert_to_euro(const gnc_commodity * currency, gnc_numeric value) {

  gnc_euro_rate_struct *result;
  const char *namespace;

  if (currency == NULL)
    return gnc_numeric_zero ();

  namespace = gnc_commodity_get_namespace (currency);
  if (namespace == NULL)
    return gnc_numeric_zero ();

  if (strcmp (namespace, GNC_COMMODITY_NS_ISO) != 0)
    return gnc_numeric_zero ();

  result = bsearch(currency,
                   _gnc_euro_rate_,
                   sizeof(_gnc_euro_rate_) / sizeof(gnc_euro_rate_struct), 
                   sizeof(gnc_euro_rate_struct),
                   _gnc_euro_rate_compare_);

  if (result == NULL)
    return gnc_numeric_zero ();

  /* round to 2 decimal places */
  {
    gnc_numeric rate;

    rate = double_to_gnc_numeric (result->rate, 100000, GNC_RND_ROUND);

    return gnc_numeric_div (value, rate, 100, GNC_RND_FLOOR);
  }
}

/* ------------------------------------------------------ */

gnc_numeric
gnc_convert_from_euro(const gnc_commodity * currency, gnc_numeric value) {

  gnc_euro_rate_struct * result;
  const char *namespace;

  if (currency == NULL)
    return gnc_numeric_zero ();

  namespace = gnc_commodity_get_namespace (currency);
  if (namespace == NULL)
    return gnc_numeric_zero ();

  if (strcmp (namespace, GNC_COMMODITY_NS_ISO) != 0)
    return gnc_numeric_zero ();

  result = bsearch(currency,
                   _gnc_euro_rate_,
                   sizeof(_gnc_euro_rate_) / sizeof(gnc_euro_rate_struct), 
                   sizeof(gnc_euro_rate_struct),
                   _gnc_euro_rate_compare_);

  if (result == NULL)
    return gnc_numeric_zero ();

  {
    gnc_numeric rate;

    rate = double_to_gnc_numeric (result->rate, 100000, GNC_RND_ROUND);

    return gnc_numeric_mul (value, rate, gnc_commodity_get_fraction (currency),
                            GNC_RND_ROUND);
  }
}

/* ------------------------------------------------------ */

const gnc_commodity *
gnc_get_euro (void)
{
  return gnc_commodity_table_lookup (gnc_engine_commodities (),
                                     GNC_COMMODITY_NS_ISO,
                                     "EUR");
}

/************************** END OF FILE *************************/
