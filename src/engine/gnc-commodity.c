/********************************************************************
 * gnc-commodity.c -- api for tradable commodities (incl. currency) *
 * Copyright (C) 2000 Bill Gribble                                  *
 * Copyright (C) 2001,2003 Linas Vepstas <linas@linas.org>          *
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
#include "gnc-trace.h"
#include "guid.h"
#include "qofbook.h"
#include "qofobject.h"

static short module = MOD_ENGINE; 

/* Parts per unit is nominal, i.e. number of 'partname' units in
 * a 'unitname' unit.  fraction is transactional, i.e. how many
 * of the smallest-transactional-units of the currency are there
 * in a 'unitname' unit. */ 

struct gnc_commodity_s 
{ 
  char    * fullname;  
  char    * namespace;
  char    * mnemonic;
  char    * printname;
  char    * exchange_code;  /* CUSIP or other identifying code */
  int       fraction;
  char    * unique_name;  
  gint16    mark;           /* user-defined mark, handy for traversals */

  gboolean  quote_flag;	    /* user wants price quotes */
  char    * quote_source;   /* current/old source of quotes */
  char    * quote_tz;
};

struct gnc_commodity_namespace_s {
  GHashTable * table;
};

struct gnc_commodity_table_s {
  GHashTable * table;
};

typedef struct gnc_commodity_namespace_s gnc_commodity_namespace;

struct gnc_new_iso_code {
  const char *old_code;
  const char *new_code;
} gnc_new_iso_codes[] = {
  {"RUB", "RUR"}, // Russian Ruble
  {"PLZ", "PLN"}, // Polish Zloty
};
#define GNC_NEW_ISO_CODES \
        (sizeof(gnc_new_iso_codes) / sizeof(struct gnc_new_iso_code))
  
/********************************************************************
 * gnc_commodity_new
 ********************************************************************/

static void
reset_printname(gnc_commodity *com)
{
    g_free(com->printname);
    com->printname = g_strdup_printf("%s (%s)",
                                     com->mnemonic ? com->mnemonic : "",
                                     com->fullname ? com->fullname : "");
}

static void
reset_unique_name(gnc_commodity *com)
{
    g_free(com->unique_name);
    com->unique_name = g_strdup_printf("%s::%s",
                                       com->namespace ? com->namespace : "",
                                       com->mnemonic ? com->mnemonic : "");
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
  retval->mark = 0;

  reset_printname(retval);
  reset_unique_name(retval);

  return retval;
}


/********************************************************************
 * gnc_commodity_destroy
 ********************************************************************/

void
gnc_commodity_destroy(gnc_commodity * cm)
{
  if(!cm) return;

  /* Set at creation */
  g_free(cm->fullname);
  cm->fullname = NULL;

  g_free(cm->namespace);
  cm->namespace = NULL;

  g_free(cm->exchange_code);
  cm->exchange_code = NULL;

  g_free(cm->mnemonic);
  cm->mnemonic = NULL;

  /* Set through accessor functions */
  g_free(cm->quote_source);
  cm->quote_source = NULL;

  g_free(cm->quote_tz);
  cm->quote_tz = NULL;

  /* Automatically generated */
  g_free(cm->printname);
  cm->printname = NULL;

  g_free(cm->unique_name);
  cm->unique_name = NULL;

  cm->mark = 0;

  g_free(cm);
}


/********************************************************************
 * gnc_commodity_get_mnemonic
 ********************************************************************/

const char *
gnc_commodity_get_mnemonic(const gnc_commodity * cm) 
{
  if(!cm) return NULL;
  return cm->mnemonic;
}

/********************************************************************
 * gnc_commodity_get_printname
 ********************************************************************/

const char *
gnc_commodity_get_printname(const gnc_commodity * cm) 
{
  if(!cm) return NULL;
  return cm->printname;
}


/********************************************************************
 * gnc_commodity_get_namespace
 ********************************************************************/

const char *
gnc_commodity_get_namespace(const gnc_commodity * cm) 
{
  if(!cm) return NULL;
  return cm->namespace;
}


/********************************************************************
 * gnc_commodity_get_fullname
 ********************************************************************/

const char *
gnc_commodity_get_fullname(const gnc_commodity * cm) 
{
  if(!cm) return NULL;
  return cm->fullname;
}


/********************************************************************
 * gnc_commodity_get_unique_name
 ********************************************************************/

const char *
gnc_commodity_get_unique_name(const gnc_commodity * cm) 
{
  if(!cm) return NULL;
  return cm->unique_name;
}


/********************************************************************
 * gnc_commodity_get_exchange_code
 ********************************************************************/

const char * 
gnc_commodity_get_exchange_code(const gnc_commodity * cm) 
{
  if(!cm) return NULL;
  return cm->exchange_code;
}

/********************************************************************
 * gnc_commodity_get_fraction
 ********************************************************************/

int
gnc_commodity_get_fraction(const gnc_commodity * cm) 
{
  if(!cm) return 0;
  return cm->fraction;
}

/********************************************************************
 * gnc_commodity_get_mark
 ********************************************************************/

gint16
gnc_commodity_get_mark(const gnc_commodity * cm) 
{
  if(!cm) return 0;
  return cm->mark;
}

/********************************************************************
 * gnc_commodity_get_quote_flag
 ********************************************************************/

gboolean
gnc_commodity_get_quote_flag(const gnc_commodity *cm)
{
  if(!cm) return FALSE;
  return (cm->quote_flag);
}

/********************************************************************
 * gnc_commodity_get_quote_source
 ********************************************************************/

const char*
gnc_commodity_get_quote_source(const gnc_commodity *cm)
{
  if(!cm) return NULL;
  if (!cm->quote_source && gnc_commodity_is_iso(cm))
    return "CURRENCY";
  return cm->quote_source;
}

/********************************************************************
 * gnc_commodity_get_quote_tz
 ********************************************************************/

const char*
gnc_commodity_get_quote_tz(const gnc_commodity *cm) 
{
  if(!cm) return NULL;
  return cm->quote_tz;
}

/********************************************************************
 * gnc_commodity_set_mnemonic
 ********************************************************************/

void
gnc_commodity_set_mnemonic(gnc_commodity * cm, const char * mnemonic) 
{
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
gnc_commodity_set_namespace(gnc_commodity * cm, const char * namespace) 
{
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
gnc_commodity_set_fullname(gnc_commodity * cm, const char * fullname) 
{
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
                                const char * exchange_code) 
{
  if(!cm) return;
  if(cm->exchange_code == exchange_code) return;

  g_free(cm->exchange_code);
  cm->exchange_code = g_strdup(exchange_code);
}

/********************************************************************
 * gnc_commodity_set_fraction
 ********************************************************************/

void
gnc_commodity_set_fraction(gnc_commodity * cm, int fraction) 
{
  if(!cm) return;
  cm->fraction = fraction;
}

/********************************************************************
 * gnc_commodity_set_mark
 ********************************************************************/

void
gnc_commodity_set_mark(gnc_commodity * cm, gint16 mark) 
{
  if(!cm) return;
  cm->mark = mark;
}

/********************************************************************
 * gnc_commodity_set_quote_flag
 ********************************************************************/

void
gnc_commodity_set_quote_flag(gnc_commodity *cm, const gboolean flag)
{
  ENTER ("(cm=%p, flag=%d)", cm, flag);

  if(!cm) return;
  cm->quote_flag = flag;
  LEAVE(" ");
}

/********************************************************************
 * gnc_commodity_set_quote_source
 ********************************************************************/

void
gnc_commodity_set_quote_source(gnc_commodity *cm, const char *src)
{
  ENTER ("(cm=%p, src=%s)", cm, src);

  if(!cm) return;
  if (cm->quote_source) {
    g_free(cm->quote_source);
    cm->quote_source = NULL;
  }

  if (src && *src)
    cm->quote_source = g_strdup(src);
  LEAVE(" ");
}

/********************************************************************
 * gnc_commodity_set_quote_tz
 ********************************************************************/

void
gnc_commodity_set_quote_tz(gnc_commodity *cm, const char *tz) 
{
  ENTER ("(cm=%p, tz=%s)", cm, tz);

  if(!cm) return;

  if (cm->quote_tz) {
    g_free(cm->quote_tz);
    cm->quote_tz = NULL;
  }

  if (tz && *tz)
    cm->quote_tz = g_strdup(tz);
  LEAVE(" ");
}

/********************************************************************\
\********************************************************************/


/********************************************************************
 * gnc_commodity_equiv
 * are two commodities the same? 
 ********************************************************************/

gboolean
gnc_commodity_equiv(const gnc_commodity * a, const gnc_commodity * b) 
{
  if(a == b) return TRUE;
  if(!a || !b) return FALSE;
  if(safe_strcmp(a->namespace, b->namespace) != 0) return FALSE;
  if(safe_strcmp(a->mnemonic, b->mnemonic) != 0) return FALSE;
  return TRUE;
}

gboolean
gnc_commodity_equal(const gnc_commodity * a, const gnc_commodity * b)
{
  if (a == b) return TRUE;

  if (!a || !b)
  {
    DEBUG ("one is NULL");
    return FALSE;
  }

  if (safe_strcmp(a->namespace, b->namespace) != 0)
  {
    DEBUG ("namespaces differ: %s vs %s", a->namespace, b->namespace);
    return FALSE;
  }

  if (safe_strcmp(a->mnemonic, b->mnemonic) != 0)
  {
    DEBUG ("mnemonics differ: %s vs %s", a->mnemonic, b->mnemonic);
    return FALSE;
  }

  if (safe_strcmp(a->fullname, b->fullname) != 0)
  {
    DEBUG ("fullnames differ: %s vs %s", a->fullname, b->fullname);
    return FALSE;
  }

  if (safe_strcmp(a->exchange_code, b->exchange_code) != 0)
  {
    DEBUG ("exchange codes differ: %s vs %s",
           a->exchange_code, b->exchange_code);
    return FALSE;
  }

  if (a->fraction != b->fraction)
  {
    DEBUG ("fractions differ: %d vs %d", a->fraction, b->fraction);
    return FALSE;
  }

  return TRUE;
}


/********************************************************************
 * gnc_commodity_table_new
 * make a new commodity table 
 ********************************************************************/

gnc_commodity_table *
gnc_commodity_table_new(void) 
{
  gnc_commodity_table * retval = g_new0(gnc_commodity_table, 1);
  retval->table = g_hash_table_new(&g_str_hash, &g_str_equal);
  return retval;
}

/********************************************************************
 * book anchor functons
 ********************************************************************/

#define GNC_COMMODITY_TABLE "gnc_commodity_table"
gnc_commodity_table *
gnc_commodity_table_get_table(QofBook *book)
{
  if (!book) return NULL;
  return qof_book_get_data (book, GNC_COMMODITY_TABLE);
}

void
gnc_commodity_table_set_table(QofBook *book, gnc_commodity_table *ct)
{
  gnc_commodity_table *old_ct;
  if (!book) return;

  old_ct = gnc_commodity_table_get_table (book);
  if (old_ct == ct) return;
  qof_book_set_data (book, GNC_COMMODITY_TABLE, ct);
  gnc_commodity_table_destroy (old_ct);
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
                           const char * namespace, const char * mnemonic)
{
  gnc_commodity_namespace * nsp = NULL;
  unsigned int i;

  if (!table || !namespace || !mnemonic) return NULL;

  nsp = g_hash_table_lookup(table->table, (gpointer)namespace);

  if(nsp) {
    /*
     * Backward compatability support for currencies that have
     * recently changed.
     */
    for (i = 0; i < GNC_NEW_ISO_CODES; i++) {
      if (strcmp(mnemonic, gnc_new_iso_codes[i].old_code) == 0) {
	mnemonic = gnc_new_iso_codes[i].new_code;
	break;
      }
    }
    return g_hash_table_lookup(nsp->table, (gpointer)mnemonic);
  }
  else {
    return NULL;
  }
}

/********************************************************************
 * gnc_commodity_table_lookup
 * locate a commodity by unique name.
 ********************************************************************/

gnc_commodity *
gnc_commodity_table_lookup_unique(const gnc_commodity_table *table,
                                  const char * unique_name)
{
  char *namespace;
  char *mnemonic;
  gnc_commodity *commodity;

  if (!table || !unique_name) return NULL;

  namespace = g_strdup (unique_name);
  mnemonic = strstr (namespace, "::");
  if (!mnemonic)
  {
    g_free (namespace);
    return NULL;
  }

  *mnemonic = '\0';
  mnemonic += 2;

  commodity = gnc_commodity_table_lookup (table, namespace, mnemonic);

  g_free (namespace);

  return commodity;
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
                           gnc_commodity * comm) 
{
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
    gnc_commodity_set_quote_flag (c, gnc_commodity_get_quote_flag (comm));
    gnc_commodity_set_quote_source (c, gnc_commodity_get_quote_source (comm));
    gnc_commodity_set_quote_tz (c, gnc_commodity_get_quote_tz (comm));
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
 * see if the commodities namespace exists. May have zero commodities.
 ********************************************************************/

int
gnc_commodity_table_has_namespace(const gnc_commodity_table * table,
                                  const char * namespace) 
{
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
hash_keys_helper(gpointer key, gpointer value, gpointer data) 
{
  GList ** l = data;
  *l = g_list_prepend(*l, key);
}

static GList *
g_hash_table_keys(GHashTable * table) 
{
  GList * l = NULL;
  g_hash_table_foreach(table, &hash_keys_helper, (gpointer) &l);
  return l;
}

static void 
hash_values_helper(gpointer key, gpointer value, gpointer data) 
{
  GList ** l = data;
  *l = g_list_prepend(*l, value);
}

static GList *
g_hash_table_values(GHashTable * table) 
{
  GList * l = NULL;
  g_hash_table_foreach(table, &hash_values_helper, (gpointer) &l);
  return l;
}

/********************************************************************
 * gnc_commodity_table_get_namespaces
 * see if any commodities in the namespace exist 
 ********************************************************************/

GList * 
gnc_commodity_table_get_namespaces(const gnc_commodity_table * table) 
{
  if (!table)
    return NULL;

  return g_hash_table_keys(table->table);
}

gboolean
gnc_commodity_namespace_is_iso(const char *namespace)
{
  return (safe_strcmp(namespace, GNC_COMMODITY_NS_ISO) == 0);
}

gboolean
gnc_commodity_is_iso(const gnc_commodity * cm)
{
  if (!cm) return FALSE;
  return (safe_strcmp(cm->namespace, GNC_COMMODITY_NS_ISO) == 0);
}

/********************************************************************
 * gnc_commodity_table_get_commodities
 * list commodities in a give namespace 
 ********************************************************************/

GList * 
gnc_commodity_table_get_commodities(const gnc_commodity_table * table,
                                    const char * namespace) 
{
  gnc_commodity_namespace * ns = NULL; 

  if (!table)
    return NULL;

  ns = g_hash_table_lookup(table->table, (gpointer)namespace);
  if (!ns)
    return NULL;

  return g_hash_table_values(ns->table);
}

/********************************************************************
 * gnc_commodity_table_get_quotable_commodities
 * list commodities in a given namespace that get price quotes
 ********************************************************************/

static void 
get_quotables_helper1(gpointer key, gpointer value, gpointer data) 
{
  gnc_commodity *comm = value; 
  GList ** l = data;

  if (!comm->quote_flag)
    return;
  *l = g_list_prepend(*l, value);
}

static gboolean 
get_quotables_helper2 (gnc_commodity *comm, gpointer data)
{
  GList ** l = data;

  if (!comm->quote_flag)
    return TRUE;
  *l = g_list_prepend(*l, comm);
  return TRUE;
}

GList * 
gnc_commodity_table_get_quotable_commodities(const gnc_commodity_table * table,
					     const char * namespace)
{
  gnc_commodity_namespace * ns = NULL; 
  GList * l = NULL;

  ENTER("table=%p, namespace=%s", table, namespace);
  if (!table)
    return NULL;

  if (namespace && *namespace) {
    ns = g_hash_table_lookup(table->table, (gpointer)namespace);
    DEBUG("ns=%p", ns);
    if (ns)
      g_hash_table_foreach(ns->table, &get_quotables_helper1, (gpointer) &l);
  } else {
    gnc_commodity_table_foreach_commodity(table, get_quotables_helper2,
					  (gpointer) &l);
  }
  LEAVE("list head %p", l);
  return l;
}

/********************************************************************
 * gnc_commodity_table_add_namespace
 * add an empty namespace if it does not exist 
 ********************************************************************/

void 
gnc_commodity_table_add_namespace(gnc_commodity_table * table,
                                  const char * namespace) 
{
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
ns_helper(gpointer key, gpointer value, gpointer user_data) 
{
  gnc_commodity * c = value;
  gnc_commodity_destroy(c);
  g_free(key);
  return TRUE;
}

void 
gnc_commodity_table_delete_namespace(gnc_commodity_table * table,
                                     const char * namespace) 
{
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

/********************************************************************
 * gnc_commodity_table_foreach_commodity
 * call user-defined function once for every commodity in every
 * namespace 
 ********************************************************************/

typedef struct {
  gboolean ok;
  gboolean (*func)(gnc_commodity *, gpointer);
  gpointer user_data;
} IterData;

static void
iter_commodity (gpointer key, gpointer value, gpointer user_data)
{
  IterData *iter_data = (IterData *) user_data;
  gnc_commodity *cm = (gnc_commodity *) value;

  if (iter_data->ok) 
  {
    iter_data->ok = (iter_data->func)(cm, iter_data->user_data);
  }
}

static void
iter_namespace (gpointer key, gpointer value, gpointer user_data)
{
  GHashTable *namespace_hash = ((gnc_commodity_namespace *) value)->table;
  g_hash_table_foreach (namespace_hash, iter_commodity, user_data);
}

gboolean 
gnc_commodity_table_foreach_commodity (const gnc_commodity_table * tbl,
                          gboolean (*f)(gnc_commodity *, gpointer),
                          gpointer user_data)
{
  IterData iter_data;

  if (!tbl || !f) return FALSE;

  iter_data.ok = TRUE;
  iter_data.func = f;
  iter_data.user_data = user_data;

  g_hash_table_foreach(tbl->table, iter_namespace, (gpointer)&iter_data);

  return iter_data.ok;
}

/********************************************************************
 * gnc_commodity_table_destroy
 * cleanup and free. 
 ********************************************************************/

static int
ct_helper(gpointer key, gpointer value, gpointer data) 
{
  gnc_commodity_namespace * ns = value;
  g_hash_table_foreach_remove(ns->table, ns_helper, NULL);
  g_hash_table_destroy(ns->table);
  ns->table = NULL;
  g_free(ns);
  g_free(key);
  return TRUE;
}

void
gnc_commodity_table_destroy(gnc_commodity_table * t) 
{
  if (!t) return;
  
  g_hash_table_foreach_remove(t->table, ct_helper, t);
  g_hash_table_destroy(t->table);
  g_free(t);
}

static gboolean 
table_equal_helper (gnc_commodity *cm_1, gpointer user_data)
{
  gnc_commodity_table *t_2 = user_data;
  gnc_commodity *cm_2;

  cm_2 = gnc_commodity_table_lookup (t_2,
                                     gnc_commodity_get_namespace (cm_1),
                                     gnc_commodity_get_mnemonic (cm_1));

  if (!cm_2)
  {
    PWARN ("one has commodity %s, the other does not",
           gnc_commodity_get_unique_name (cm_1));
    return FALSE;
  }

  return gnc_commodity_equal (cm_1, cm_2);
}

gboolean
gnc_commodity_table_equal(gnc_commodity_table *t_1,
                          gnc_commodity_table *t_2)
{
  gboolean ok;

  if (t_1 == t_2) return TRUE;
  if (!t_1 || !t_2) return FALSE;

  ok = gnc_commodity_table_foreach_commodity (t_1, table_equal_helper, t_2);
  if (!ok)
    return FALSE;

  return gnc_commodity_table_foreach_commodity (t_2, table_equal_helper, t_1);
}

/********************************************************************
 * gnc_commodity_table_add_default_data
 ********************************************************************/

gboolean
gnc_commodity_table_add_default_data(gnc_commodity_table *table)
{

  #include "iso-4217-currencies.c"

  gnc_commodity_table_add_namespace(table, GNC_COMMODITY_NS_AMEX);
  gnc_commodity_table_add_namespace(table, GNC_COMMODITY_NS_NYSE);
  gnc_commodity_table_add_namespace(table, GNC_COMMODITY_NS_NASDAQ);
  gnc_commodity_table_add_namespace(table, GNC_COMMODITY_NS_EUREX);
  gnc_commodity_table_add_namespace(table, GNC_COMMODITY_NS_MUTUAL);
  return TRUE;
}

/********************************************************************
 ********************************************************************/
/* gncObject function implementation and registration */

static void 
commodity_table_book_begin (QofBook *book)
{
  gnc_commodity_table *ct;
  
  ct = gnc_commodity_table_new ();
  if(!gnc_commodity_table_add_default_data(ct))
  {
    PWARN("unable to initialize book's commodity_table");
  }
  gnc_commodity_table_set_table (book, ct);
  
}

static void 
commodity_table_book_end (QofBook *book)
{
  gnc_commodity_table_set_table (book, NULL);
}

/* XXX Why is the commodity table never marked dirty/clean?
 * Don't we have to save user-created/modified commodities?
 * I don't get it ... does this need fixing?
 */
static QofObject commodity_table_object_def = 
{
  interface_version: QOF_OBJECT_VERSION,
  name:              GNC_ID_COMMODITY_TABLE,
  type_label:        "CommodityTable",
  book_begin:        commodity_table_book_begin,
  book_end:          commodity_table_book_end,
  is_dirty:          NULL,
  mark_clean:        NULL,
  foreach:           NULL,
  printable:         NULL,
};

gboolean 
gnc_commodity_table_register (void)
{
  return qof_object_register (&commodity_table_object_def);
}

/* ========================= END OF FILE ============================== */
