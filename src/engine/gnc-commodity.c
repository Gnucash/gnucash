/********************************************************************
 * gnc-commodity.c -- api for tradable commodities (incl. currency) *
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

#define _GNU_SOURCE

#include "config.h"

#include <limits.h>
#include <string.h>
#include <stdio.h>
#include <glib.h>

#include "gnc-commodity.h"
#include "gnc-engine-util.h"


/* parts per unit is nominal, i.e. number of 'partname' units in
 * a 'unitname' unit.  fraction is transactional, i.e. how many
 * of the smallest-transactional-units of the currency are there
 * in a 'unitname' unit. */ 

struct _gnc_commodity { 
  char   * fullname;  
  char   * namespace;
  char   * mnemonic;
  char   * printname;
  char   * exchange_code;  /* CUSIP or other identifying code */
  int    fraction;
  char   * unique_name;  
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

static void
reset_printname(gnc_commodity *com)
{
    if(com->printname) { g_free(com->printname); }
    com->printname = g_strdup_printf("%s (%s)",
                                     com->mnemonic,
                                     com->fullname);
}

static void
reset_unique_name(gnc_commodity *com)
{
    if(com->unique_name) { g_free(com->unique_name); }
    com->unique_name = g_strdup_printf("%s::%s",
                                       com->namespace,
                                       com->mnemonic);
}
    
gnc_commodity *
gnc_commodity_new(const char * fullname, 
                  const char * namespace, const char * mnemonic, 
                  const char * exchange_code, 
                  int fraction)
{
  gnc_commodity * retval = g_new0(gnc_commodity, 1);
  
  retval->fullname  = g_strdup(fullname);
  retval->namespace = g_strdup(namespace);
  retval->mnemonic  = g_strdup(mnemonic);
  retval->exchange_code = g_strdup(exchange_code);
  retval->fraction = fraction;

  reset_printname(retval);
  reset_unique_name(retval);
  
  return retval;
}


/********************************************************************
 * gnc_commodity_destroy
 ********************************************************************/

void
gnc_commodity_destroy(gnc_commodity * cm) {
  if(!cm) return;
  g_free(cm->fullname);
  g_free(cm->printname);
  g_free(cm->namespace);
  g_free(cm->exchange_code);
  g_free(cm->mnemonic);
  g_free(cm->unique_name);
  g_free(cm);
}


/********************************************************************
 * gnc_commodity_get_mnemonic
 ********************************************************************/

const char *
gnc_commodity_get_mnemonic(const gnc_commodity * cm) {
  if(!cm) return NULL;
  return cm->mnemonic;
}

/********************************************************************
 * gnc_commodity_get_printname
 ********************************************************************/

const char *
gnc_commodity_get_printname(const gnc_commodity * cm) {
  if(!cm) return NULL;
  return cm->printname;
}


/********************************************************************
 * gnc_commodity_get_namespace
 ********************************************************************/

const char *
gnc_commodity_get_namespace(const gnc_commodity * cm) {
  if(!cm) return NULL;
  return cm->namespace;
}


/********************************************************************
 * gnc_commodity_get_fullname
 ********************************************************************/

const char *
gnc_commodity_get_fullname(const gnc_commodity * cm) {
  if(!cm) return NULL;
  return cm->fullname;
}


/********************************************************************
 * gnc_commodity_get_unique_name
 ********************************************************************/

const char *
gnc_commodity_get_unique_name(const gnc_commodity * cm) {
  if(!cm) return NULL;
  return cm->unique_name;
}


/********************************************************************
 * gnc_commodity_get_exchange_code
 ********************************************************************/

const char * 
gnc_commodity_get_exchange_code(const gnc_commodity * cm) {
  if(!cm) return NULL;
  return cm->exchange_code;
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
  if(cm->mnemonic == mnemonic) return;

  g_free(cm->mnemonic);
  cm->mnemonic = g_strdup(mnemonic);

  reset_printname(cm);
  reset_unique_name(cm);
}

/********************************************************************
 * gnc_commodity_set_namespace
 ********************************************************************/

void
gnc_commodity_set_namespace(gnc_commodity * cm, const char * namespace) {
  if(!cm) return;
  if(cm->namespace == namespace) return;

  g_free(cm->namespace);
  cm->namespace = g_strdup(namespace);

  reset_printname(cm);
  reset_unique_name(cm);
}

/********************************************************************
 * gnc_commodity_set_fullname
 ********************************************************************/

void
gnc_commodity_set_fullname(gnc_commodity * cm, const char * fullname) {
  if(!cm) return;
  if(cm->fullname == fullname) return;

  g_free(cm->fullname);
  cm->fullname = g_strdup(fullname);

  reset_printname(cm);
}

/********************************************************************
 * gnc_commodity_set_exchange_code
 ********************************************************************/

void
gnc_commodity_set_exchange_code(gnc_commodity * cm, 
                                const char * exchange_code) {
  if(!cm) return;
  if(cm->exchange_code == exchange_code) return;

  g_free(cm->exchange_code);
  cm->exchange_code = g_strdup(exchange_code);
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

gboolean
gnc_commodity_equiv(const gnc_commodity * a, const gnc_commodity * b) {
  if(a == b) return TRUE;
  if(!a || !b) return FALSE;
  if(safe_strcmp(a->namespace, b->namespace) != 0) return FALSE;
  if(safe_strcmp(a->mnemonic, b->mnemonic) != 0) return FALSE;
  return TRUE;
}


/********************************************************************
 * gnc_commodity_table_new
 * make a new commodity table 
 ********************************************************************/

gnc_commodity_table *
gnc_commodity_table_new(void) {
  gnc_commodity_table * retval = g_new0(gnc_commodity_table, 1);
  retval->table = g_hash_table_new(&g_str_hash, &g_str_equal);
  return retval;
}

/********************************************************************
 * gnc_commodity_get_size
 * get the size of the commodity table
 ********************************************************************/

guint
gnc_commodity_table_get_number_of_namespaces(gnc_commodity_table* tbl)
{
    g_return_val_if_fail(tbl, 0);
    g_return_val_if_fail(tbl->table, 0);
    return g_hash_table_size(tbl->table);
}

static void
count_coms(gpointer key, gpointer value, gpointer user_data)
{
    GHashTable *tbl = ((gnc_commodity_namespace*)value)->table;
    guint *count = (guint*)user_data;

    if(safe_strcmp((char*)key, GNC_COMMODITY_NS_ISO) == 0)
    {
        /* don't count default commodities */
        return;
    }
    
    if(!value) return;
    
    *count += g_hash_table_size(tbl);
}

guint
gnc_commodity_table_get_size(gnc_commodity_table* tbl)
{
    guint count = 0;
    g_return_val_if_fail(tbl, 0);
    g_return_val_if_fail(tbl->table, 0);

    g_hash_table_foreach(tbl->table, count_coms, (gpointer)&count);

    return count;
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

  if (!fullname || (fullname[0] == '\0'))
    return NULL;

  all = gnc_commodity_table_get_commodities(table, namespace);

  for(iterator = all; iterator; iterator=iterator->next) {
    if(!strcmp(fullname, 
               gnc_commodity_get_printname(iterator->data))) {
      retval = iterator->data;
      break;
    }
  }

  g_list_free (all);

  return retval;
}


/********************************************************************
 * gnc_commodity_table_insert
 * add a commodity to the table. 
 ********************************************************************/

gnc_commodity *
gnc_commodity_table_insert(gnc_commodity_table * table, 
                           gnc_commodity * comm) {
  gnc_commodity_namespace * nsp = NULL;
  gnc_commodity *c;

  if (!table) return NULL;
  if (!comm) return NULL;

  c = gnc_commodity_table_lookup (table, comm->namespace, comm->mnemonic);

  if (c) {
    if (c == comm)
      return c;

    gnc_commodity_set_fullname (c, gnc_commodity_get_fullname (comm));
    gnc_commodity_set_fraction (c, gnc_commodity_get_fraction (comm));
    gnc_commodity_set_exchange_code (c,
                                     gnc_commodity_get_exchange_code (comm));

    gnc_commodity_destroy (comm);

    return c;
  }

  nsp = g_hash_table_lookup(table->table, (gpointer)(comm->namespace));
  
  if(!nsp) {
    nsp = g_new0(gnc_commodity_namespace, 1);
    nsp->table = g_hash_table_new(g_str_hash, g_str_equal);
    g_hash_table_insert(table->table, 
                        g_strdup(comm->namespace), 
                        (gpointer)nsp);
  }

  g_hash_table_insert(nsp->table, 
                      (gpointer)g_strdup(comm->mnemonic),
                      (gpointer)comm);

  return comm;
}

/********************************************************************
 * gnc_commodity_table_remove
 * remove a commodity from the table. 
 ********************************************************************/

void
gnc_commodity_table_remove(gnc_commodity_table * table,
                           gnc_commodity * comm)
{
  gnc_commodity_namespace * nsp;
  gnc_commodity *c;

  if (!table) return;
  if (!comm) return;

  c = gnc_commodity_table_lookup (table, comm->namespace, comm->mnemonic);
  if (c != comm) return;

  nsp = g_hash_table_lookup (table->table, comm->namespace);
  if (!nsp) return;

  g_hash_table_remove (nsp->table, comm->mnemonic);
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
  *l = g_list_prepend(*l, key);
}

static GList *
g_hash_table_keys(GHashTable * table) {
  GList * l = NULL;
  g_hash_table_foreach(table, &hash_keys_helper, (gpointer) &l);
  return l;
}

static void 
hash_values_helper(gpointer key, gpointer value, gpointer data) {
  GList ** l = data;
  *l = g_list_prepend(*l, value);
}

static GList *
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
  if (!table)
    return NULL;

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
    g_hash_table_insert(table->table,
                        (gpointer) g_strdup(namespace), 
                        (gpointer) ns);
  }
}


/********************************************************************
 * gnc_commodity_table_delete_namespace
 * delete a namespace  
 ********************************************************************/

static int
ns_helper(gpointer key, gpointer value, gpointer user_data) {
  gnc_commodity * c = value;
  gnc_commodity_destroy(c);
  g_free(key);
  return TRUE;
}

void 
gnc_commodity_table_delete_namespace(gnc_commodity_table * table,
                                     const char * namespace) {
  gpointer orig_key;
  gnc_commodity_namespace * value;

  if(table) { 
    if(g_hash_table_lookup_extended(table->table,
                                    (gpointer) namespace,
                                    &orig_key,
                                    (gpointer)&value)) {
      g_hash_table_remove(table->table, namespace);

      g_hash_table_foreach_remove(value->table, ns_helper, NULL);
      g_hash_table_destroy(value->table);
      g_free(value);

      g_free(orig_key);
    }
  }
}

void
gnc_commodity_table_remove_non_iso (gnc_commodity_table *t)
{
  GList *namespaces;
  GList *node;

  if (!t) return;

  namespaces = gnc_commodity_table_get_namespaces (t);

  for (node = namespaces; node; node = node->next)
  {
    char *ns = node->data;

    if (safe_strcmp (ns, GNC_COMMODITY_NS_ISO) == 0)
      continue;

    gnc_commodity_table_delete_namespace (t, ns);
  }

  g_list_free (namespaces);
}

/********************************************************************
 * gnc_commodity_table_destroy
 * cleanup and free. 
 ********************************************************************/

static int
ct_helper(gpointer key, gpointer value, gpointer data) {
  gnc_commodity_namespace * ns = value;
  g_hash_table_foreach_remove(ns->table, ns_helper, NULL);
  g_hash_table_destroy(ns->table);
  ns->table = NULL;
  g_free(ns);
  g_free(key);
  return TRUE;
}

void
gnc_commodity_table_destroy(gnc_commodity_table * t) {
  if (!t) return;
  
  g_hash_table_foreach_remove(t->table, ct_helper, t);
  g_hash_table_destroy(t->table);
  g_free(t);
}
