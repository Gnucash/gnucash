/********************************************************************
 * gnc_commodity.h -- representing tradable commodities             *
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
#define GNC_COMMODITY_NS_ISO    "ISO-4217 Currencies"
#define GNC_COMMODITY_NS_NASDAQ "NASDAQ Securities"
#define GNC_COMMODITY_NS_NYSE   "NYSE Securities"
#define GNC_COMMODITY_NS_EUREX  "EUREX Securities"
#define GNC_COMMODITY_NS_MUTUAL "US Mutual Funds"

/* gnc_commodity functions */
gnc_commodity * gnc_commodity_new(const char * fullname, 
                                  const char * unitname,
                                  const char * partname,
                                  const char * namespace,
                                  const char * mnemonic,
                                  int  exchange_code,
                                  int  parts_per_unit,
                                  int  smallest_fraction);
     
void  gnc_commodity_destroy(gnc_commodity * cm);

char  * gnc_commodity_get_mnemonic(const gnc_commodity * cm);
char  * gnc_commodity_get_namespace(const gnc_commodity * cm);
char  * gnc_commodity_get_fullname(const gnc_commodity * cm);
char  * gnc_commodity_get_unitname(const gnc_commodity * cm);
char  * gnc_commodity_get_partname(const gnc_commodity * cm);
char  * gnc_commodity_get_printname(const gnc_commodity * cm);
int   gnc_commodity_get_exchange_code(const gnc_commodity * cm);
int   gnc_commodity_get_parts_per_unit(const gnc_commodity * cm);
int   gnc_commodity_get_fraction(const gnc_commodity * cm);

void  gnc_commodity_set_mnemonic(gnc_commodity * cm, const char * mnemonic);
void  gnc_commodity_set_namespace(gnc_commodity * cm, const char * namespace);
void  gnc_commodity_set_fullname(gnc_commodity * cm, const char * fullname);
void  gnc_commodity_set_unitname(gnc_commodity * cm, const char * unitname);
void  gnc_commodity_set_partname(gnc_commodity * cm, const char * partname);
void  gnc_commodity_set_exchange_code(gnc_commodity * cm, int exchange_code);
void  gnc_commodity_set_parts_per_unit(gnc_commodity * cm, int parts);
void  gnc_commodity_set_fraction(gnc_commodity * cm, int smallest_fraction);

int   gnc_commodity_equiv(const gnc_commodity * a, const gnc_commodity * b);


/* gnc_commodity_table functions : operate on a database of commodity
 * info */

gnc_commodity_table * gnc_commodity_table_new();
void          gnc_commodity_table_destroy();
gnc_commodity * gnc_commodity_table_lookup(const gnc_commodity_table * table, 
                                           const char * namespace, 
                                           const char * mnemonic);
gnc_commodity * gnc_commodity_table_find_full(const gnc_commodity_table * t,
                                              const char * namespace,
                                              const char * fullname);
void          gnc_commodity_table_insert(gnc_commodity_table * table,
                                         const gnc_commodity * comm);
void          gnc_commodity_table_remove(gnc_commodity_table * table,
                                         const gnc_commodity * comm);

int       gnc_commodity_table_has_namespace(const gnc_commodity_table * t,
                                            const char * namespace);
GList     * gnc_commodity_table_get_namespaces(const gnc_commodity_table * t);
GList     * gnc_commodity_table_get_commodities(const gnc_commodity_table * t,
                                                const char * namespace);
void      gnc_commodity_table_add_namespace(gnc_commodity_table * table,
                                            const char * namespace);
void      gnc_commodity_table_delete_namespace(gnc_commodity_table * t,
                                               const char * namespace);
#endif


