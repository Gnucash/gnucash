/********************************************************************\
 * gnc-euro.c -- utilities for EURO currency                        *
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

#include "config.h"

#include "gnc-euro.h"

#include <math.h>
#include <stdlib.h>
#include <string.h>

#include "app-utils/gnc-ui-util.h"

/* local structs */
typedef struct
{
    const char *currency;
    double rate;
} gnc_euro_rate_struct;


/* This array MUST be sorted ! */
/* The rates are per EURO */
static gnc_euro_rate_struct gnc_euro_rates[] =
{
    { "ATS",  13.7603 },  /* austrian schilling */
    { "BEF",  40.3399 },  /* belgian franc */
    { "BFR",  40.3399 },  /* belgian franc */
    { "CYP",  .585274 },  /* cyprus pound */
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
    { "MTL",  .429300 },  /* maltese lira */
    { "NLG",  2.20371 },  /* netherland gulden */
    { "PTA",  166.386 },  /* spanish peseta */
    { "PTE",  200.482 },  /* portugese escudo */
    { "S",    13.7603 },  /* austrian schilling */
    { "SCH",  13.7603 },  /* austrian schilling */
    { "SIT",  239.640 },  /* slovenian tolar */
    { "SKK",  30.1260 }   /* slovak koruna */
};

static int
gnc_euro_rate_compare (const void * key, const void * value)
{
    const gnc_commodity * curr = key;
    const gnc_euro_rate_struct * euro = value;

    if (!key || !value)
        return -1;

    return g_ascii_strcasecmp(gnc_commodity_get_mnemonic(curr), euro->currency);
}

static int
gnc_euro_rate_compare_code (const void * key, const void * value)
{
    const char *code = key;
    const gnc_euro_rate_struct * euro = value;

    if (!key || !value)
        return -1;

    return g_ascii_strcasecmp (code, euro->currency);
}

/* ------------------------------------------------------ */

gboolean
gnc_is_euro_currency_code (const char *code)
{
    gnc_euro_rate_struct *result;

    if (!code) return FALSE;

    result = bsearch (code,
                      gnc_euro_rates,
                      sizeof(gnc_euro_rates) / sizeof(gnc_euro_rate_struct),
                      sizeof(gnc_euro_rate_struct),
                      gnc_euro_rate_compare_code);

    return result != NULL;
}

gboolean
gnc_is_euro_currency(const gnc_commodity * currency)
{
    gnc_euro_rate_struct *result;

    if (currency == NULL)
        return FALSE;

    if (!gnc_commodity_is_iso(currency))
        return FALSE;

    result = bsearch(currency,
                     gnc_euro_rates,
                     sizeof(gnc_euro_rates) / sizeof(gnc_euro_rate_struct),
                     sizeof(gnc_euro_rate_struct),
                     gnc_euro_rate_compare);

    if (result == NULL)
        return FALSE;

    return TRUE;
}

/* ------------------------------------------------------ */

gnc_numeric
gnc_convert_to_euro(const gnc_commodity * currency, gnc_numeric value)
{
    gnc_euro_rate_struct *result;

    if (currency == NULL)
        return gnc_numeric_zero ();

    if (!gnc_commodity_is_iso(currency))
        return gnc_numeric_zero ();

    result = bsearch(currency,
                     gnc_euro_rates,
                     sizeof(gnc_euro_rates) / sizeof(gnc_euro_rate_struct),
                     sizeof(gnc_euro_rate_struct),
                     gnc_euro_rate_compare);

    if (result == NULL)
        return gnc_numeric_zero ();

    /* round to 2 decimal places */
    {
        gnc_numeric rate;

        rate = double_to_gnc_numeric (result->rate, 100000, GNC_HOW_RND_ROUND_HALF_UP);

        /* EC Regulation 1103/97 states we should use "Round half away from zero"
         * See http://europa.eu/legislation_summaries/economic_and_monetary_affairs/institutional_and_economic_framework/l25025_en.htm */
        return gnc_numeric_div (value, rate, 100, GNC_HOW_RND_ROUND_HALF_UP);
    }
}

/* ------------------------------------------------------ */

gnc_numeric
gnc_convert_from_euro(const gnc_commodity * currency, gnc_numeric value)
{
    gnc_euro_rate_struct * result;

    if (currency == NULL)
        return gnc_numeric_zero ();

    if (!gnc_commodity_is_iso(currency))
        return gnc_numeric_zero ();

    result = bsearch(currency,
                     gnc_euro_rates,
                     sizeof(gnc_euro_rates) / sizeof(gnc_euro_rate_struct),
                     sizeof(gnc_euro_rate_struct),
                     gnc_euro_rate_compare);

    if (result == NULL)
        return gnc_numeric_zero ();

    {
        gnc_numeric rate;

        rate = double_to_gnc_numeric (result->rate, 100000, GNC_HOW_RND_ROUND_HALF_UP);

        /* EC Regulation 1103/97 states we should use "Round half away from zero"
         * See http://europa.eu/legislation_summaries/economic_and_monetary_affairs/institutional_and_economic_framework/l25025_en.htm */
        return gnc_numeric_mul (value, rate, gnc_commodity_get_fraction (currency),
                                GNC_HOW_RND_ROUND_HALF_UP);
    }
}

/* ------------------------------------------------------ */

gnc_numeric
gnc_euro_currency_get_rate (const gnc_commodity *currency)
{
    gnc_euro_rate_struct * result;

    if (currency == NULL)
        return gnc_numeric_zero ();

    if (!gnc_commodity_is_iso(currency))
        return gnc_numeric_zero ();

    result = bsearch(currency,
                     gnc_euro_rates,
                     sizeof(gnc_euro_rates) / sizeof(gnc_euro_rate_struct),
                     sizeof(gnc_euro_rate_struct),
                     gnc_euro_rate_compare);

    if (result == NULL)
        return gnc_numeric_zero ();

    return double_to_gnc_numeric (result->rate, GNC_DENOM_AUTO,
                                  GNC_HOW_DENOM_SIGFIGS(6) | GNC_HOW_RND_ROUND_HALF_UP);
}

/* ------------------------------------------------------ */

gnc_commodity *
gnc_get_euro (void)
{
    gnc_commodity_table *table;

    table = gnc_commodity_table_get_table (gnc_get_current_book ());

    return gnc_commodity_table_lookup (table, GNC_COMMODITY_NS_CURRENCY, "EUR");
}
