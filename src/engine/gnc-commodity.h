/********************************************************************
 * gnc-commodity.h -- api for tradable commodities (incl. currency) *
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

#ifndef __GNC_COMMODITY_H__
#define __GNC_COMMODITY_H__

#include <glib.h>

typedef struct _gnc_commodity           gnc_commodity;
typedef struct _gnc_commodity_table     gnc_commodity_table;

#define GNC_COMMODITY_NS_LEGACY "GNC_LEGACY_CURRENCIES"
#define GNC_COMMODITY_NS_ISO    "ISO4217"
#define GNC_COMMODITY_NS_NASDAQ "NASDAQ"
#define GNC_COMMODITY_NS_NYSE   "NYSE"
#define GNC_COMMODITY_NS_EUREX  "EUREX"
#define GNC_COMMODITY_NS_MUTUAL "FUND"
#define GNC_COMMODITY_NS_AMEX   "AMEX"

/* gnc_commodity functions */
gnc_commodity * gnc_commodity_new(const char * fullname, 
                                  const char * namespace,
                                  const char * mnemonic,
                                  const char * exchange_code,
                                  int fraction);

void  gnc_commodity_destroy(gnc_commodity * cm);

const char * gnc_commodity_get_mnemonic(const gnc_commodity * cm);
const char * gnc_commodity_get_namespace(const gnc_commodity * cm);
const char * gnc_commodity_get_fullname(const gnc_commodity * cm);
const char * gnc_commodity_get_printname(const gnc_commodity * cm);
const char * gnc_commodity_get_exchange_code(const gnc_commodity * cm);
const char * gnc_commodity_get_unique_name(const gnc_commodity * cm);
int   gnc_commodity_get_fraction(const gnc_commodity * cm);

void  gnc_commodity_set_mnemonic(gnc_commodity * cm, const char * mnemonic);
void  gnc_commodity_set_namespace(gnc_commodity * cm, const char * namespace);
void  gnc_commodity_set_fullname(gnc_commodity * cm, const char * fullname);
void  gnc_commodity_set_exchange_code(gnc_commodity * cm, 
                                      const char * exchange_code);
void  gnc_commodity_set_fraction(gnc_commodity * cm, int smallest_fraction);

gboolean gnc_commodity_equiv(const gnc_commodity * a, const gnc_commodity * b);


/* gnc_commodity_table functions : operate on a database of commodity
 * info */

gnc_commodity_table * gnc_commodity_table_new(void);
void          gnc_commodity_table_destroy(gnc_commodity_table * table);
gnc_commodity * gnc_commodity_table_lookup(const gnc_commodity_table * table, 
                                           const char * namespace, 
                                           const char * mnemonic);
gnc_commodity * gnc_commodity_table_find_full(const gnc_commodity_table * t,
                                              const char * namespace,
                                              const char * fullname);
gnc_commodity * gnc_commodity_table_insert(gnc_commodity_table * table,
                                           gnc_commodity * comm);

int       gnc_commodity_table_has_namespace(const gnc_commodity_table * t,
                                            const char * namespace);

/* The next two functions return newly allocated lists which should
 * be freed with g_list_free. */
GList     * gnc_commodity_table_get_namespaces(const gnc_commodity_table * t);
GList     * gnc_commodity_table_get_commodities(const gnc_commodity_table * t,
                                                const char * namespace);

void      gnc_commodity_table_add_namespace(gnc_commodity_table * table,
                                            const char * namespace);
void      gnc_commodity_table_delete_namespace(gnc_commodity_table * t,
                                               const char * namespace);
#endif


