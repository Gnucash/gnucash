/********************************************************************
 * gnc_commodity.c -- representing tradable commodities             *
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

#include <limits.h>
#include <string.h>
#include <stdint.h>
#include <stdio.h>

#include "gnc-commodity.h"

/* parts per unit is nominal, i.e. number of 'partname' units in a 
 * 'unitname' unit.  fraction is transactional, i.e. how many 
 * of the smallest-transactional-units of the currency are there 
 * in a 'unitname' unit. */ 

struct _gnc_commodity { 
  char   * fullname;  
  char   * unitname;
  char   * partname;
  char   * namespace;
  char   * mnemonic;
  char   * printname;
  int    exchange_code;
  int    parts_per_unit;  
  int    fraction;
};

struct _gnc_commodity_namespace {
  GHashTable * table;
};

struct _gnc_commodity_table {
  GHashTable * table;
};

typedef struct _gnc_commodity_namespace gnc_commodity_namespace;

/********************************************************************
 * gnc_commodity_new
 ********************************************************************/

gnc_commodity *
gnc_commodity_new(const char * fullname, 
                  const char * unitname, const char * partname,
                  const char * namespace, const char * mnemonic, 
                  int exchange_code, int parts_per_unit, int fraction) {
  
  gnc_commodity * retval = g_new0(gnc_commodity, 1);

  retval->fullname  = g_strdup(fullname);
  retval->unitname  = g_strdup(unitname);
  retval->partname  = g_strdup(partname);
  retval->namespace = g_strdup(namespace);
  retval->mnemonic  = g_strdup(mnemonic);
  retval->exchange_code     = exchange_code;
  retval->parts_per_unit    = parts_per_unit;
  retval->fraction = fraction;
  
  asprintf(&retval->printname, "%s (%s)", 
           retval->mnemonic, retval->fullname);
  
  return retval;
}


/********************************************************************
 * gnc_commodity_destroy
 ********************************************************************/

void
gnc_commodity_destroy(gnc_commodity * cm) {
  if(!cm) return;
  g_free(cm->fullname);
  g_free(cm->unitname);
  g_free(cm->partname);
  g_free(cm->namespace);
  g_free(cm->mnemonic);
  g_free(cm);
}


/********************************************************************
 * gnc_commodity_get_mnemonic
 ********************************************************************/

char *
gnc_commodity_get_mnemonic(const gnc_commodity * cm) {
  if(!cm) return NULL;
  return cm->mnemonic;
}

/********************************************************************
 * gnc_commodity_get_printname
 ********************************************************************/

char *
gnc_commodity_get_printname(const gnc_commodity * cm) {
  if(!cm) return NULL;
  return cm->printname;
}


/********************************************************************
 * gnc_commodity_get_namespace
 ********************************************************************/

char *
gnc_commodity_get_namespace(const gnc_commodity * cm) {
  if(!cm) return NULL;
  return cm->namespace;
}


/********************************************************************
 * gnc_commodity_get_fullname
 ********************************************************************/

char *
gnc_commodity_get_fullname(const gnc_commodity * cm) {
  if(!cm) return NULL;
  return cm->fullname;
}



/********************************************************************
 * gnc_commodity_get_partname
 ********************************************************************/

char *
gnc_commodity_get_partname(const gnc_commodity * cm) {
  if(!cm) return NULL;
  return cm->partname;
}


/********************************************************************
 * gnc_commodity_get_unitname
 ********************************************************************/

char *
gnc_commodity_get_unitname(const gnc_commodity * cm) {
  if(!cm) return NULL;
  return cm->unitname;
}

/********************************************************************
 * gnc_commodity_get_exchange_code
 ********************************************************************/

int
gnc_commodity_get_exchange_code(const gnc_commodity * cm) {
  if(!cm) return -1;
  return cm->exchange_code;
}


/********************************************************************
 * gnc_commodity_get_parts_per_unit
 ********************************************************************/

int
gnc_commodity_get_parts_per_unit(const gnc_commodity * cm) {
  if(!cm) return 0;
  return cm->parts_per_unit;
}

/********************************************************************
 * gnc_commodity_get_fraction
 ********************************************************************/

int
gnc_commodity_get_fraction(const gnc_commodity * cm) {
  if(!cm) return 0;
  return cm->fraction;
}


/********************************************************************
 * gnc_commodity_set_mnemonic
 ********************************************************************/

void
gnc_commodity_set_mnemonic(gnc_commodity * cm, const char * mnemonic) {
  if(!cm) return;
  cm->mnemonic = g_strdup(mnemonic);

  asprintf(&cm->printname, "%s (%s)", 
           cm->mnemonic, cm->fullname);
}

/********************************************************************
 * gnc_commodity_set_namespace
 ********************************************************************/

void
gnc_commodity_set_namespace(gnc_commodity * cm, const char * namespace) {
  if(!cm) return;
  cm->namespace = g_strdup(namespace);
}

/********************************************************************
 * gnc_commodity_set_fullname
 ********************************************************************/

void
gnc_commodity_set_fullname(gnc_commodity * cm, const char * fullname) {
  if(!cm) return;
  cm->fullname = g_strdup(fullname);
  asprintf(&cm->printname, "%s (%s)", 
           cm->mnemonic, cm->fullname);
}

/********************************************************************
 * gnc_commodity_set_unitname
 ********************************************************************/

void
gnc_commodity_set_unitname(gnc_commodity * cm, const char * unitname) {
  if(!cm) return;
  cm->unitname = g_strdup(unitname);
}


/********************************************************************
 * gnc_commodity_set_partname
 ********************************************************************/

void
gnc_commodity_set_partname(gnc_commodity * cm, const char * partname) {
  if(!cm) return;
  cm->partname = g_strdup(partname);
}

/********************************************************************
 * gnc_commodity_set_exchange_code
 ********************************************************************/

void
gnc_commodity_set_exchange_code(gnc_commodity * cm, int exchange_code) {
  if(!cm) return;
  cm->exchange_code = exchange_code;
}

/********************************************************************
 * gnc_commodity_set_parts_per_unit
 ********************************************************************/

void
gnc_commodity_set_parts_per_unit(gnc_commodity * cm, int parts_per_unit) {
  if(!cm) return;
  cm->parts_per_unit = parts_per_unit;
}


/********************************************************************
 * gnc_commodity_set_fraction
 ********************************************************************/

void
gnc_commodity_set_fraction(gnc_commodity * cm, int fraction) {
  if(!cm) return;
  cm->fraction = fraction;
}


/********************************************************************
 * gnc_commodity_equiv
 * are two commodities the same? 
 ********************************************************************/

int
gnc_commodity_equiv(const gnc_commodity * a, const gnc_commodity * b) {
  if(a && b && (a == b)) { 
    return 1; 
  }
  else if(a && b && a->namespace && b->namespace &&
          !strcmp(a->namespace, b->namespace) &&
          a->mnemonic && b->mnemonic &&
          !strcmp(a->mnemonic, b->mnemonic)) {
    return 1;
  }
  else {
    return 0;
  }
}


/********************************************************************
 * gnc_commodity_table_new
 * make a new commodity table 
 ********************************************************************/

gnc_commodity_table *
gnc_commodity_table_new() {
  gnc_commodity_table * retval = g_new0(gnc_commodity_table, 1);
  retval->table = g_hash_table_new(&g_str_hash, &g_str_equal);
  return retval;
}


/********************************************************************
 * gnc_commodity_table_lookup
 * locate a commodity by namespace and mnemonic. 
 ********************************************************************/

gnc_commodity *
gnc_commodity_table_lookup(const gnc_commodity_table * table, 
                           const char * namespace, const char * mnemonic) {
  gnc_commodity_namespace * nsp = NULL;

  nsp = g_hash_table_lookup(table->table, (gpointer)namespace);

  if(nsp) {
    return g_hash_table_lookup(nsp->table, (gpointer)mnemonic);
  }
  else {
    return NULL;
  }
}


/********************************************************************
 * gnc_commodity_table_find_full
 * locate a commodity by namespace and printable name 
 ********************************************************************/

gnc_commodity *
gnc_commodity_table_find_full(const gnc_commodity_table * table, 
                              const char * namespace, 
                              const char * fullname) {  
  gnc_commodity * retval=NULL;
  GList         * all;
  GList         * iterator;

  all = gnc_commodity_table_get_commodities(table, namespace);
  if(fullname && fullname[0]) {
    for(iterator = all; iterator; iterator=iterator->next) {
      if(!strcmp(fullname, 
                 gnc_commodity_get_printname(iterator->data))) {
        retval = iterator->data;
        break;
      }
    }
    return retval;
  }
  else {
    return NULL;
  }
}


/********************************************************************
 * gnc_commodity_table_insert
 * add a commodity to the table. 
 ********************************************************************/

void
gnc_commodity_table_insert(gnc_commodity_table * table, 
                           const gnc_commodity * comm) {
  gnc_commodity_namespace * nsp = NULL;

  nsp = g_hash_table_lookup(table->table, (gpointer)(comm->namespace));
  
  if(!nsp) {
    nsp = g_new0(gnc_commodity_namespace, 1);
    nsp->table = g_hash_table_new(g_str_hash, g_str_equal);
    g_hash_table_insert(table->table, (gpointer)(comm->namespace), 
                        (gpointer)nsp);
  }

  return g_hash_table_insert(nsp->table, 
                             (gpointer)comm->mnemonic,
                             (gpointer)comm);  
}

/********************************************************************
 * gnc_commodity_table_remove
 * remove a commodity from the table (just unmaps it)
 ********************************************************************/

void
gnc_commodity_table_remove(gnc_commodity_table * table, 
                           const gnc_commodity * comm) {
  gnc_commodity_namespace * nsp = NULL;

  nsp = g_hash_table_lookup(table->table, (gpointer)(comm->namespace));
  
  if(nsp) {
    g_hash_table_remove(nsp->table, (gpointer)comm->mnemonic);
  }
}

/********************************************************************
 * gnc_commodity_table_has_namespace
 * see if any commodities in the namespace exist 
 ********************************************************************/

int
gnc_commodity_table_has_namespace(const gnc_commodity_table * table,
                                  const char * namespace) {
  gnc_commodity_namespace * nsp = NULL;
  
  if(!table || !namespace) { return 0; }

  nsp = g_hash_table_lookup(table->table, (gpointer)namespace);
  if(nsp) {
    return 1;
  }
  else {
    return 0;
  }
}

static void 
hash_keys_helper(gpointer key, gpointer value, gpointer data) {
  GList ** l = data;
  *l = g_list_append(*l, key);
}

GList *
g_hash_table_keys(GHashTable * table) {
  GList * l = NULL;
  g_hash_table_foreach(table, &hash_keys_helper, (gpointer) &l);
  return l;
}

static void 
hash_values_helper(gpointer key, gpointer value, gpointer data) {
  GList ** l = data;
  *l = g_list_append(*l, value);
}

GList *
g_hash_table_values(GHashTable * table) {
  GList * l = NULL;
  g_hash_table_foreach(table, &hash_values_helper, (gpointer) &l);
  return l;
}

/********************************************************************
 * gnc_commodity_table_get_namespaces
 * see if any commodities in the namespace exist 
 ********************************************************************/

GList * 
gnc_commodity_table_get_namespaces(const gnc_commodity_table * table) {
  return g_hash_table_keys(table->table);
}
    

/********************************************************************
 * gnc_commodity_table_get_commodities
 * list commodities in a give namespace 
 ********************************************************************/

GList * 
gnc_commodity_table_get_commodities(const gnc_commodity_table * table,
                                    const char * namespace) {
  gnc_commodity_namespace * ns = NULL; 
  
  if(table) { 
    ns = g_hash_table_lookup(table->table, (gpointer)namespace);
  }
  
  if(ns) {
    return g_hash_table_values(ns->table);
  }
  else {
    return NULL;
  }
}

/********************************************************************
 * gnc_commodity_table_add_namespace
 * add an empty namespace if it does not exist 
 ********************************************************************/

void 
gnc_commodity_table_add_namespace(gnc_commodity_table * table,
                                  const char * namespace) {
  gnc_commodity_namespace * ns = NULL; 
  
  if(table) { 
    ns = g_hash_table_lookup(table->table, (gpointer)namespace);
  }
  
  if(!ns) {
    ns = g_new0(gnc_commodity_namespace, 1);
    ns->table = g_hash_table_new(g_str_hash, g_str_equal);
    g_hash_table_insert(table->table, (gpointer)(namespace), 
                        (gpointer)ns);
  }
}

/********************************************************************
 * gnc_commodity_table_delete_namespace
 * delete a namespace  
 ********************************************************************/

void 
gnc_commodity_table_delete_namespace(gnc_commodity_table * table,
                                     const char * namespace) {
  gnc_commodity_namespace * ns = NULL; 
  
  if(table) { 
    ns = g_hash_table_lookup(table->table, (gpointer)namespace);
  }
  
  if(ns) {
    g_hash_table_remove(table->table, namespace);
  }
}
    


