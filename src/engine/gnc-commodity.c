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

#include <ctype.h>
#include <limits.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <glib.h>
#include <regex.h>

#include "gnc-commodity.h"
#include "gnc-engine-util.h"
#include "gnc-trace.h"
#include "guid.h"
#include "messages.h"
#include "qofbook.h"
#include "qofobject.h"

static short module = MOD_COMMODITY; 

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
  gnc_quote_source * quote_source;   /* current/old source of quotes */
  char    * quote_tz;
};

struct gnc_commodity_namespace_s 
{
  char    * namespace;
  GHashTable * table;
};

struct gnc_commodity_table_s 
{
  GHashTable * table;
};

typedef struct gnc_commodity_namespace_s gnc_commodity_namespace;

struct gnc_new_iso_code
{
  const char *old_code;
  const char *new_code;
} gnc_new_iso_codes[] = {
  {"RUB", "RUR"}, // Russian Ruble
  {"PLZ", "PLN"}, // Polish Zloty
};
#define GNC_NEW_ISO_CODES \
        (sizeof(gnc_new_iso_codes) / sizeof(struct gnc_new_iso_code))

static gboolean fq_is_installed = FALSE;

struct gnc_quote_source_s {
  gboolean supported;
  QuoteSourceType type;
  gint index;
  char *user_name;		/* User friendly name */
  char *old_internal_name;	/* Name used internally (deprecated) */
  char *internal_name;		/* Name used internally and by finance::quote. */
};

static gnc_quote_source currency_quote_source =
  { TRUE, 0, 0, "CURRENCY", "currency", "currency" };

static gnc_quote_source single_quote_sources[] = {
  { FALSE, 0, 0, "AEX", "AEX", "aex" },
  { FALSE, 0, 0, "ASX", "ASX", "asx" },
  { FALSE, 0, 0, "DWS", "DWS", "dwsfunds" },
  { FALSE, 0, 0, "Fidelity Direct", "FIDELITY_DIRECT", "fidelity_direct" },
  { FALSE, 0, 0, "Motley Fool", "FOOL", "fool" },
  { FALSE, 0, 0, "Fund Library", "FUNDLIBRARY", "fundlibrary" },
  { FALSE, 0, 0, "TD Waterhouse Canada", "TDWATERHOUSE", "tdwaterhouse" },
  { FALSE, 0, 0, "TIAA-CREF", "TIAACREF", "tiaacref" },
  { FALSE, 0, 0, "T. Rowe Price", "TRPRICE_DIRECT", "troweprice_direct" },
  { FALSE, 0, 0, "Trustnet", "TRUSTNET", "trustnet" },
  { FALSE, 0, 0, "Union Investments", "UNIONFUNDS", "unionfunds" },
  { FALSE, 0, 0, "Vanguard", "VANGUARD", "vanguard" },
  { FALSE, 0, 0, "VWD", "VWD", "vwd" },
  { FALSE, 0, 0, "Yahoo", "YAHOO", "yahoo" },
  { FALSE, 0, 0, "Yahoo Asia", "YAHOO_ASIA", "yahoo_asia" },
  { FALSE, 0, 0, "Yahoo Australia", "YAHOO_AUSTRALIA", "yahoo_australia" },
  { FALSE, 0, 0, "Yahoo Europe", "YAHOO_EUROPE", "yahoo_europe" },
  { FALSE, 0, 0, "Zuerich Investments", "ZIFUNDS", "zifunds" },
};
static gnc_quote_source multiple_quote_sources[] = {
  { FALSE, 0, 0, "Asia (Yahoo, ...)", "ASIA", "asia" },
  { FALSE, 0, 0, "Australia (ASX, Yahoo, ...)", "AUSTRALIA", "australia" },
  { FALSE, 0, 0, "Canada (Yahoo, ...)", "CANADA", "canada" },
  { FALSE, 0, 0, "Canada Mutual (Fund Library, ...)", "CANADAMUTUAL", "canadamutual" },
  { FALSE, 0, 0, "Dutch (AEX, ...)", "DUTCH", "dutch" },
  { FALSE, 0, 0, "Europe (Yahoo, ...)", "EUROPE", "europe" },
  { FALSE, 0, 0, "Fidelity (Fidelity, ...)", "FIDELITY", "fidelity" },
  { FALSE, 0, 0, "Nasdaq (Yahoo, ...)", "NASDAQ", "nasdaq" },
  { FALSE, 0, 0, "NYSE (Yahoo, ...)", "NYSE", "nyse" },
  { FALSE, 0, 0, "T. Rowe Price", "TRPRICE", "troweprice" },
  { FALSE, 0, 0, "U.K. Unit Trusts", "UKUNITTRUSTS", "uk_unit_trusts" },
  { FALSE, 0, 0, "USA (Yahoo, Fool ...)", "USA", "usa" },
};

static const int num_single_quote_sources =
	sizeof(single_quote_sources) / sizeof(gnc_quote_source);
static const int num_multiple_quote_sources =
	sizeof(multiple_quote_sources) / sizeof(gnc_quote_source);
static GList *new_quote_sources = NULL;


/********************************************************************
 * gnc_quote_source_fq_installed
 *
 * This function indicates whether or not the Finance::Quote module
 * is installed on a users computer.
 ********************************************************************/
gboolean
gnc_quote_source_fq_installed (void)
{
  return fq_is_installed;
}

/********************************************************************
 * gnc_quote_source_num_entries
 *
 * Return the number of entries for a given type of price source.
 ********************************************************************/
gint gnc_quote_source_num_entries(QuoteSourceType type)
{
  if  (type == SOURCE_CURRENCY)
    return 1;
  
  if  (type == SOURCE_SINGLE)
    return num_single_quote_sources;
  
  if (type == SOURCE_MULTI)
    return num_multiple_quote_sources;

  return g_list_length(new_quote_sources);
}

/********************************************************************
 * gnc_quote_source_init_tables
 *
 * Update the type/index values for prices sources.
 ********************************************************************/
static void
gnc_quote_source_init_tables (void)
{
  gint i;

  for (i = 0; i < num_single_quote_sources; i++) {
    single_quote_sources[i].type = SOURCE_SINGLE;
    single_quote_sources[i].index = i;
  }

  for (i = 0; i < num_multiple_quote_sources; i++) {
    multiple_quote_sources[i].type = SOURCE_MULTI;
    multiple_quote_sources[i].index = i;
  }

  currency_quote_source.type = SOURCE_CURRENCY;
  currency_quote_source.index = 0;
}


/********************************************************************
 * gnc_quote_source_add_new
 *
 * Add a new price source. Called when unknown source names are found
 * either in the F::Q installation (a newly available source) or in
 * the user's data file (a source that has vanished but needs to be
 * tracked.)
 ********************************************************************/
gnc_quote_source *
gnc_quote_source_add_new (const char *source_name, gboolean supported)
{
  gnc_quote_source *new_source;

  DEBUG("Creating new source %s", source_name);
  new_source = malloc(sizeof(gnc_quote_source));
  new_source->supported = supported;
  new_source->type = SOURCE_UNKNOWN;
  new_source->index = g_list_length(new_quote_sources);

  /* This name can be changed if/when support for this price source is
   * integrated into gnucash. */
  new_source->user_name = strdup(source_name);

  /* This name is permanent and must be kept the same if/when support
   * for this price source is integrated into gnucash (i.e. for a
   * nice user name). */
  new_source->old_internal_name = strdup(source_name);
  new_source->internal_name = strdup(source_name);
  new_quote_sources = g_list_append(new_quote_sources, new_source);
  return new_source;
}

/********************************************************************
 * gnc_quote_source_lookup_by_xxx
 *
 * Lookup a price source data structure based upon various criteria.
 ********************************************************************/
gnc_quote_source *
gnc_quote_source_lookup_by_ti (QuoteSourceType type, gint index)
{
  gnc_quote_source *source;
  GList *node;

  ENTER("type/index is %d/%d", type, index);
  switch (type) {
   case SOURCE_CURRENCY:
    LEAVE("found %s", currency_quote_source.user_name);
    return &currency_quote_source;
    break;

   case SOURCE_SINGLE:
    if (index < num_single_quote_sources) {
      LEAVE("found %s", single_quote_sources[index].user_name);
      return &single_quote_sources[index];
    }
    break;

   case SOURCE_MULTI:
    if (index < num_multiple_quote_sources) {
      LEAVE("found %s", multiple_quote_sources[index].user_name);
      return &multiple_quote_sources[index];
    }
    break;

   case SOURCE_UNKNOWN:
   default:
    node = g_list_nth(new_quote_sources, index);
    if (node) {
      source = node->data;
      LEAVE("found %s", source->user_name);
      return source;
    }
  }

  LEAVE("not found");
  return NULL;
}

gnc_quote_source *
gnc_quote_source_lookup_by_internal(const char * name)
{
  gnc_quote_source *source;
  GList *node;
  gint i;

  if ((name == NULL) || (safe_strcmp(name, "") == 0)) {
    return NULL;
  }

  if (safe_strcmp(name, currency_quote_source.internal_name) == 0)
    return &currency_quote_source;
  if (safe_strcmp(name, currency_quote_source.old_internal_name) == 0)
    return &currency_quote_source;

  for (i = 0; i < num_single_quote_sources; i++) {
    if (safe_strcmp(name, single_quote_sources[i].internal_name) == 0)
      return &single_quote_sources[i];
    if (safe_strcmp(name, single_quote_sources[i].old_internal_name) == 0)
      return &single_quote_sources[i];
  }

  for (i = 0; i < num_multiple_quote_sources; i++) {
    if (safe_strcmp(name, multiple_quote_sources[i].internal_name) == 0)
      return &multiple_quote_sources[i];
    if (safe_strcmp(name, multiple_quote_sources[i].old_internal_name) == 0)
      return &multiple_quote_sources[i];
  }

  for (i = 0, node = new_quote_sources; node; node = node->next, i++) {
    source = node->data;
    if (safe_strcmp(name, source->internal_name) == 0)
      return source;
    if (safe_strcmp(name, source->old_internal_name) == 0)
      return source;
  }

  LEAVE("Unknown source %s", name);
  return NULL;
}

/********************************************************************
 * gnc_quote_source_get_xxx
 *
 * Accessor functions - get functions only. There are no set functions.
 ********************************************************************/
QuoteSourceType
gnc_quote_source_get_type (gnc_quote_source *source)
{
  ENTER("%p", source);
  if (!source) {
    LEAVE("bad source");
    return SOURCE_SINGLE;
  }

  DEBUG("type is %d",source->type);
  return source->type;
}

gint
gnc_quote_source_get_index (gnc_quote_source *source)
{
  ENTER("%p", source);
  if (!source) {
    LEAVE("bad source");
    return 0;
  }

  DEBUG("index is %d", source->index);
  return source->index;
}

gboolean
gnc_quote_source_get_supported (gnc_quote_source *source)
{
  ENTER("%p", source);
  if (!source) {
    LEAVE("bad source");
    return FALSE;
  }

  DEBUG("%ssupported", source && source->supported ? "" : "not ");
  return source->supported;
}

const char *
gnc_quote_source_get_user_name (gnc_quote_source *source)
{
  ENTER("%p", source);
  if (!source) {
    LEAVE("bad source");
    return NULL;
  }
  LEAVE("user name %s", source->user_name);
  return source->user_name;
}

const char *
gnc_quote_source_get_old_internal_name (gnc_quote_source *source)
{
  ENTER("%p", source);
  if (!source) {
    LEAVE("bad source");
    return NULL;
  }
  LEAVE("old internal name %s", source->old_internal_name);
  return source->old_internal_name;
}

const char *
gnc_quote_source_get_internal_name (gnc_quote_source *source)
{
  ENTER("%p", source);
  if (!source) {
    LEAVE("bad source");
    return NULL;
  }
  LEAVE("internal name %s", source->internal_name);
  return source->internal_name;
}

/********************************************************************
 * gnc_quote_source_set_fq_installed
 *
 * Update gnucash internal tables on what Finance::Quote sources are
 * installed.
 ********************************************************************/
void
gnc_quote_source_set_fq_installed (GList *sources_list)
{
  gnc_quote_source *source;
  char *source_name;
  GList *node;

  ENTER(" ");
  fq_is_installed = TRUE;

  if (!sources_list)
    return;

  for (node = sources_list; node; node = node->next) {
    source_name = node->data;

    source = gnc_quote_source_lookup_by_internal(source_name);
    if (source != NULL) {
      DEBUG("Found source %s: %s", source_name, source->user_name);
      source->supported = TRUE;
      continue;
    }

    gnc_quote_source_add_new(source_name, TRUE);
  }
  LEAVE(" ");
}

/********************************************************************
 * gnc_commodity_new
 ********************************************************************/

#define CACHE_INSERT(dest,src)       \
  if(src) { dest = g_cache_insert(str_cache, (gpointer)src); }

#define CACHE_REMOVE(str)     \
  if(str) { g_cache_remove(str_cache, str); str = NULL; }

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
  GCache *str_cache = gnc_engine_get_string_cache ();
  gnc_commodity * retval = g_new0(gnc_commodity, 1);

  CACHE_INSERT (retval->fullname, fullname);
  CACHE_INSERT (retval->namespace, namespace);
  CACHE_INSERT (retval->mnemonic, mnemonic);
  CACHE_INSERT (retval->exchange_code, exchange_code);
  retval->fraction = fraction;
  retval->mark = 0;
  retval->quote_flag = 0;
  retval->quote_source = NULL;
  retval->quote_tz = NULL;

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
  GCache *str_cache = gnc_engine_get_string_cache ();
  if(!cm) return;

  /* Set at creation */
  CACHE_REMOVE (cm->fullname);
  CACHE_REMOVE (cm->namespace);
  CACHE_REMOVE (cm->exchange_code);
  CACHE_REMOVE (cm->mnemonic);
  CACHE_REMOVE (cm->quote_tz);

  /* Set through accessor functions */
  cm->quote_source = NULL;

  /* Automatically generated */
  g_free(cm->printname);
  cm->printname = NULL;

  g_free(cm->unique_name);
  cm->unique_name = NULL;

  cm->mark = 0;

  g_free(cm);
}

void
gnc_commodity_copy(gnc_commodity * dest, gnc_commodity *src)
{
  gnc_commodity_set_fullname (dest, src->fullname);
  gnc_commodity_set_namespace (dest, src->namespace);
  gnc_commodity_set_fraction (dest, src->fraction);
  gnc_commodity_set_exchange_code (dest, src->exchange_code);
  gnc_commodity_set_quote_flag (dest, src->quote_flag);
  gnc_commodity_set_quote_source (dest, gnc_commodity_get_quote_source (src));
  gnc_commodity_set_quote_tz (dest, src->quote_tz);
}

gnc_commodity *
gnc_commodity_clone(gnc_commodity *src)
{
  GCache *str_cache = gnc_engine_get_string_cache ();
  gnc_commodity * dest = g_new0(gnc_commodity, 1);

  CACHE_INSERT (dest->fullname, src->fullname);
  CACHE_INSERT (dest->namespace, src->namespace);
  CACHE_INSERT (dest->mnemonic, src->mnemonic);
  CACHE_INSERT (dest->exchange_code, src->exchange_code);
  CACHE_INSERT (dest->quote_tz, src->quote_tz);

  dest->mark = 0;
  dest->fraction = src->fraction;
  dest->quote_flag = src->quote_flag;

  gnc_commodity_set_quote_source (dest, gnc_commodity_get_quote_source (src));

  reset_printname(dest);
  reset_unique_name(dest);

  return dest;
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

gnc_quote_source*
gnc_commodity_get_quote_source(const gnc_commodity *cm)
{
  if(!cm) return NULL;
  if (!cm->quote_source && gnc_commodity_is_iso(cm))
    return &currency_quote_source;
  return cm->quote_source;
}

gnc_quote_source*
gnc_commodity_get_default_quote_source(const gnc_commodity *cm)
{
  if (cm && gnc_commodity_is_iso(cm))
    return &currency_quote_source;
  /* Should make this a user option at some point. */
  return gnc_quote_source_lookup_by_internal("yahoo");
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
  GCache *str_cache = gnc_engine_get_string_cache ();
  if(!cm) return;
  if(cm->mnemonic == mnemonic) return;

  CACHE_REMOVE (cm->mnemonic);
  CACHE_INSERT (cm->mnemonic, mnemonic);

  reset_printname(cm);
  reset_unique_name(cm);
}

/********************************************************************
 * gnc_commodity_set_namespace
 ********************************************************************/

void
gnc_commodity_set_namespace(gnc_commodity * cm, const char * namespace) 
{
  GCache *str_cache = gnc_engine_get_string_cache ();
  if(!cm) return;
  if(cm->namespace == namespace) return;

  CACHE_REMOVE (cm->namespace);
  CACHE_INSERT (cm->namespace, namespace);

  reset_printname(cm);
  reset_unique_name(cm);
}

/********************************************************************
 * gnc_commodity_set_fullname
 ********************************************************************/

void
gnc_commodity_set_fullname(gnc_commodity * cm, const char * fullname) 
{
  GCache *str_cache = gnc_engine_get_string_cache ();
  if(!cm) return;
  if(cm->fullname == fullname) return;

  CACHE_REMOVE (cm->fullname);
  CACHE_INSERT (cm->fullname, fullname);

  reset_printname(cm);
}

/********************************************************************
 * gnc_commodity_set_exchange_code
 ********************************************************************/

void
gnc_commodity_set_exchange_code(gnc_commodity * cm, 
                                const char * exchange_code) 
{
  GCache *str_cache = gnc_engine_get_string_cache ();
  if(!cm) return;
  if(cm->exchange_code == exchange_code) return;

  CACHE_REMOVE (cm->exchange_code);
  CACHE_INSERT (cm->exchange_code, exchange_code);
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
gnc_commodity_set_quote_source(gnc_commodity *cm, gnc_quote_source *src)
{
  ENTER ("(cm=%p, src=%p(%s))", cm, src, src ? src->internal_name : "unknown");

  if(!cm) return;
  cm->quote_source = src;
  LEAVE(" ");
}

/********************************************************************
 * gnc_commodity_set_quote_tz
 ********************************************************************/

void
gnc_commodity_set_quote_tz(gnc_commodity *cm, const char *tz) 
{
  GCache *str_cache = gnc_engine_get_string_cache ();
  ENTER ("(cm=%p, tz=%s)", cm, tz);

  if(!cm) return;

  CACHE_REMOVE (cm->quote_tz);
  if (tz && *tz) CACHE_INSERT (cm->quote_tz, tz);

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
                              const char * fullname) 
{  
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
  GCache *str_cache;
  gnc_commodity_namespace * nsp = NULL;
  gnc_commodity *c;

  if (!table) return NULL;
  if (!comm) return NULL;

  ENTER ("(table=%p, comm=%p) %s %s", table, comm, comm->mnemonic, comm->fullname);
  c = gnc_commodity_table_lookup (table, comm->namespace, comm->mnemonic);

  if (c) 
  {
    if (c == comm)
    {
      return c;
    }
    gnc_commodity_copy (c, comm);
    gnc_commodity_destroy (comm);
    return c;
  }

  nsp = g_hash_table_lookup(table->table, (gpointer)(comm->namespace));
  str_cache = gnc_engine_get_string_cache ();
  
  if(!nsp) 
  {
    nsp = g_new0(gnc_commodity_namespace, 1);
    nsp->table = g_hash_table_new(g_str_hash, g_str_equal);
    nsp->namespace = g_cache_insert(str_cache, comm->namespace);
    g_hash_table_insert(table->table, 
                        nsp->namespace, 
                        (gpointer)nsp);
  }

  PINFO ("insert %p %s into nsp=%p %s", comm->mnemonic, comm->mnemonic, nsp->table, nsp->namespace);
  g_hash_table_insert(nsp->table, 
                      (gpointer)g_cache_insert(str_cache, comm->mnemonic),
                      (gpointer)comm);

  LEAVE ("(table=%p, comm=%p)", table, comm);
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
  /* XXX minor mem leak, should remove the key as well */
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

  if (!comm->quote_flag ||
      !comm->quote_source || !comm->quote_source->supported)
    return;
  *l = g_list_prepend(*l, value);
}

static gboolean 
get_quotables_helper2 (gnc_commodity *comm, gpointer data)
{
  GList ** l = data;

  if (!comm->quote_flag ||
      !comm->quote_source || !comm->quote_source->supported)
    return TRUE;
  *l = g_list_prepend(*l, comm);
  return TRUE;
}

GList * 
gnc_commodity_table_get_quotable_commodities(const gnc_commodity_table * table,
					     const char *expression)
{
  gnc_commodity_namespace * ns = NULL;
  const char *namespace;
  GList * nslist, * tmp;
  GList * l = NULL;
  regex_t pattern;

  ENTER("table=%p, expression=%s", table, expression);
  if (!table)
    return NULL;

  if (expression && *expression) {
    if (regcomp(&pattern, expression, REG_EXTENDED|REG_ICASE) != 0) {
      LEAVE("Cannot compile regex");
      return NULL;
    }

    nslist = gnc_commodity_table_get_namespaces(table);
    for (tmp = nslist; tmp; tmp = tmp->next) {
      namespace = tmp->data;
      if (regexec(&pattern, namespace, 0, NULL, 0) == 0) {
	DEBUG("Running list of %s commodities", namespace);
	ns = g_hash_table_lookup(table->table, namespace);
	if (ns) {
	  g_hash_table_foreach(ns->table, &get_quotables_helper1, (gpointer) &l);
	}
      }
    }
    g_list_free(nslist);
    regfree(&pattern);
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
  
  if(table) 
  { 
    ns = g_hash_table_lookup(table->table, (gpointer)namespace);
  }
  
  if(!ns) 
  {
    GCache *str_cache = gnc_engine_get_string_cache ();
    ns = g_new0(gnc_commodity_namespace, 1);
    ns->table = g_hash_table_new(g_str_hash, g_str_equal);
    ns->namespace = g_cache_insert(str_cache, (gpointer)namespace);
    g_hash_table_insert(table->table,
                        (gpointer) ns->namespace, 
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
  GCache *str_cache = user_data;
  gnc_commodity * c = value;
  gnc_commodity_destroy(c);
  g_cache_remove (str_cache, key);  /* key is commodity mnemonic */
  return TRUE;
}

void 
gnc_commodity_table_delete_namespace(gnc_commodity_table * table,
                                     const char * namespace) 
{
  gnc_commodity_namespace * ns;
  if (!table) return;

  ns = g_hash_table_lookup(table->table, namespace);
  if (ns)
  {
    GCache *str_cache = gnc_engine_get_string_cache ();
    g_hash_table_remove(table->table, namespace);

    g_hash_table_foreach_remove(ns->table, ns_helper, str_cache);
    g_hash_table_destroy(ns->table);
    g_cache_remove (str_cache, ns->namespace);
    g_free(ns);
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
  GCache *str_cache = gnc_engine_get_string_cache ();
  gnc_commodity_namespace * ns = value;

  g_hash_table_foreach_remove(ns->table, ns_helper, str_cache);
  g_hash_table_destroy(ns->table);
  ns->table = NULL;
  g_cache_remove (str_cache, ns->namespace);
  g_free(ns);
  return TRUE;
}

void
gnc_commodity_table_destroy(gnc_commodity_table * t) 
{
  if (!t) return;
  ENTER ("table=%p", t);
  
  g_hash_table_foreach_remove(t->table, ct_helper, NULL);
  g_hash_table_destroy(t->table);
  t->table = NULL;
  g_free(t);
  LEAVE ("table=%p", t);
}

/* =========================================================== */

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

/* =========================================================== */

static gboolean 
table_copy_helper (gnc_commodity *src_cm, gpointer user_data)
{
  gnc_commodity_table *dest = user_data;
  gnc_commodity_table_insert (dest, gnc_commodity_clone (src_cm));
  return TRUE;
}

void
gnc_commodity_table_copy(gnc_commodity_table *dest,
                          gnc_commodity_table *src)
{
  gnc_commodity_table_foreach_commodity (src, table_copy_helper, dest);
}

/********************************************************************
 * gnc_commodity_table_add_default_data
 ********************************************************************/

gboolean
gnc_commodity_table_add_default_data(gnc_commodity_table *table)
{
  ENTER ("table=%p", table);
  #include "iso-4217-currencies.c"

  gnc_commodity_table_add_namespace(table, GNC_COMMODITY_NS_AMEX);
  gnc_commodity_table_add_namespace(table, GNC_COMMODITY_NS_NYSE);
  gnc_commodity_table_add_namespace(table, GNC_COMMODITY_NS_NASDAQ);
  gnc_commodity_table_add_namespace(table, GNC_COMMODITY_NS_EUREX);
  gnc_commodity_table_add_namespace(table, GNC_COMMODITY_NS_MUTUAL);
  LEAVE ("table=%p", table);
  return TRUE;
}

/********************************************************************
 ********************************************************************/
/* gncObject function implementation and registration */

static void 
commodity_table_book_begin (QofBook *book)
{
  gnc_commodity_table *ct;
  ENTER ("book=%p", book);
  
  ct = gnc_commodity_table_new ();
  if(!gnc_commodity_table_add_default_data(ct))
  {
    PWARN("unable to initialize book's commodity_table");
  }
  gnc_commodity_table_set_table (book, ct);
  LEAVE ("book=%p", book);
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
  e_type:            GNC_ID_COMMODITY_TABLE,
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
  gnc_quote_source_init_tables();
  return qof_object_register (&commodity_table_object_def);
}

/* ========================= END OF FILE ============================== */
