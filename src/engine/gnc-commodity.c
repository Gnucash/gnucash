/********************************************************************
 * gnc-commodity.c -- api for tradable commodities (incl. currency) *
 * Copyright (C) 2000 Bill Gribble                                  *
 * Copyright (C) 2001,2003 Linas Vepstas <linas@linas.org>          *
 * Copyright (c) 2006 David Hampton <hampton@employees.org>         *
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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
 *******************************************************************/

#include "config.h"

#include <glib.h>
#include <glib/gi18n.h>
#include <ctype.h>
#include <limits.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <regex.h>

#include "gnc-commodity.h"
#include "gnc-main.h"

static QofLogModule log_module = GNC_MOD_COMMODITY; 

/* Parts per unit is nominal, i.e. number of 'partname' units in
 * a 'unitname' unit.  fraction is transactional, i.e. how many
 * of the smallest-transactional-units of the currency are there
 * in a 'unitname' unit. */ 

struct gnc_commodity_s 
{ 
  QofInstance inst;

  gnc_commodity_namespace *namespace;

  char    * fullname;  
  char    * mnemonic;
  char    * printname;
  char    * cusip;          /* CUSIP or other identifying code */
  int       fraction;
  char    * unique_name;  
  gint16    mark;           /* user-defined mark, handy for traversals */

  gboolean  quote_flag;	    /* user wants price quotes */
  gnc_quote_source * quote_source;   /* current/old source of quotes */
  char    * quote_tz;

  /* the number of accounts using this commodity - this field is not
   * persisted */
  int       usage_count;
};

struct _GncCommodityClass
{
  QofInstanceClass parent_class;
};

struct gnc_commodity_namespace_s 
{
  QofInstance inst;

  gchar      * name;
  gboolean     iso4217;
  GHashTable * cm_table;
  GList      * cm_list;
};

struct _GncCommodityNamespaceClass
{
  QofInstanceClass parent_class;
};

struct gnc_commodity_table_s 
{
  GHashTable * ns_table;
  GList      * ns_list;
};

struct gnc_new_iso_code
{
  const char *old_code;
  const char *new_code;
} gnc_new_iso_codes[] = {
  {"RUR", "RUB"}, /* Russian Ruble: RUR through 1997-12, RUB from 1998-01 onwards; see bug #393185 */
  {"PLZ", "PLN"}, /* Polish Zloty */
  {"UAG", "UAH"}, /* Ukraine Hryvnia */
  {"ILS", "NIS"}, /* New Israeli Shekel */
  {"MXP", "MXN"}, /* Mexican (Nuevo) Peso */
  {"TRL", "TRY"}, /* New Turkish Lira: changed 2005 */

  /* Only add currencies to this table when the old currency no longer
   * exists in the file iso-4217-currencies.scm */
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
  { TRUE, 0, 0, "Currency", "CURRENCY", "currency" };

static gnc_quote_source single_quote_sources[] = {
  { FALSE, 0, 0, "AEX", "AEX", "aex" },
  { FALSE, 0, 0, "AEX Futures", "AEX_FUTURES", "aex_futures" },
  { FALSE, 0, 0, "AEX Options", "AEX_OPTIONS", "aex_options" },
  { FALSE, 0, 0, "AMFI India", "AMFIINDIA", "amfiindia" },
  { FALSE, 0, 0, "ASE", "ASEGR", "asegr" },
  { FALSE, 0, 0, "ASX", "ASX", "asx" },
  { FALSE, 0, 0, "BMO NesbittBurns", "BMONESBITTBURNS", "bmonesbittburns" },
  { FALSE, 0, 0, "Deka Investments", "DEKA", "deka" },
  { FALSE, 0, 0, "DWS", "DWS", "dwsfunds" },
  { FALSE, 0, 0, "Fidelity Direct", "FIDELITY_DIRECT", "fidelity_direct" },
  { FALSE, 0, 0, "Finance Canada", "FINANCECANADA", "financecanada" },
  { FALSE, 0, 0, "First Trust Portfolios", "FTPORTFOLIOS_DIRECT", "ftportfolios_direct" },
  { FALSE, 0, 0, "Fund Library", "FUNDLIBRARY", "fundlibrary" },
  { FALSE, 0, 0, "Man Investments", "maninv", "maninv" },
  { FALSE, 0, 0, "Motley Fool", "FOOL", "fool" },
  { FALSE, 0, 0, "NZX", "NZX", "nzx" },
  { FALSE, 0, 0, "Platinum Asset Management", "PLATINUM", "platinum" },
  { FALSE, 0, 0, "SEB", "SEB_FUNDS", "seb_funds" },
  { FALSE, 0, 0, "Sharenet", "sharenet", "sharenet" },
  { FALSE, 0, 0, "TD Waterhouse Canada", "TDWATERHOUSE", "tdwaterhouse" },
  { FALSE, 0, 0, "TD Efunds", "TDEFUNDS", "tdefunds" },
  { FALSE, 0, 0, "TIAA-CREF", "TIAACREF", "tiaacref" },
  { FALSE, 0, 0, "T. Rowe Price", "TRPRICE_DIRECT", "troweprice_direct" },
  { FALSE, 0, 0, "Trustnet", "TRUSTNET", "trustnet" },
  { FALSE, 0, 0, "Union Investments", "UNIONFUNDS", "unionfunds" },
  { FALSE, 0, 0, "US Treasury Bonds", "usfedbonds", "usfedbonds" },
  { FALSE, 0, 0, "US Govt. Thrift Savings Plan", "TSP", "tsp" },
  { FALSE, 0, 0, "Vanguard", "VANGUARD", "vanguard" },
  { FALSE, 0, 0, "VWD", "VWD", "vwd" },
  { FALSE, 0, 0, "Yahoo", "YAHOO", "yahoo" },
  { FALSE, 0, 0, "Yahoo Asia", "YAHOO_ASIA", "yahoo_asia" },
  { FALSE, 0, 0, "Yahoo Australia", "YAHOO_AUSTRALIA", "yahoo_australia" },
  { FALSE, 0, 0, "Yahoo Brasil", "YAHOO_BRASIL", "yahoo_brasil" },
  { FALSE, 0, 0, "Yahoo Europe", "YAHOO_EUROPE", "yahoo_europe" },
  { FALSE, 0, 0, "Yahoo New Zealand", "YAHOO_NZ", "yahoo_nz" },
  { FALSE, 0, 0, "Zuerich Investments", "ZIFUNDS", "zifunds" }, /* Removed from F::Q 1.11. */
};
static gnc_quote_source multiple_quote_sources[] = {
  { FALSE, 0, 0, "Asia (Yahoo, ...)", "ASIA", "asia" },
  { FALSE, 0, 0, "Australia (ASX, Yahoo, ...)", "AUSTRALIA", "australia" },
  { FALSE, 0, 0, "Brasil (Yahoo, ...)", "BRASIL", "brasil" },
  { FALSE, 0, 0, "Canada (Yahoo, ...)", "CANADA", "canada" },
  { FALSE, 0, 0, "Canada Mutual (Fund Library, ...)", "CANADAMUTUAL", "canadamutual" },
  { FALSE, 0, 0, "Dutch (AEX, ...)", "DUTCH", "dutch" },
  { FALSE, 0, 0, "Europe (Yahoo, ...)", "EUROPE", "europe" },
  { FALSE, 0, 0, "Greece (ASE, ...)", "GREECE", "greece" },
  { FALSE, 0, 0, "India Mutual (AMFI, ...)", "INDIAMUTUAL", "indiamutual" },
  { FALSE, 0, 0, "Fidelity (Fidelity, ...)", "FIDELITY", "fidelity" },
  { FALSE, 0, 0, "First Trust (First Trust, ...)", "FTPORTFOLIOS", "ftportfolios" },
  { FALSE, 0, 0, "Nasdaq (Yahoo, ...)", "NASDAQ", "nasdaq" },
  { FALSE, 0, 0, "New Zealand (Yahoo, ...)", "NZ", "nz" },
  { FALSE, 0, 0, "NYSE (Yahoo, ...)", "NYSE", "nyse" },
  { FALSE, 0, 0, "South Africa (Sharenet, ...)", "ZA", "za" },
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

  DEBUG("Creating new source %s", (source_name == NULL ? "(null)" : source_name));
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
gnc_quote_source_get_type (const gnc_quote_source *source)
{
  ENTER("%p", source);
  if (!source) {
    LEAVE("bad source");
    return SOURCE_SINGLE;
  }

  LEAVE("type is %d",source->type);
  return source->type;
}

gint
gnc_quote_source_get_index (const gnc_quote_source *source)
{
  ENTER("%p", source);
  if (!source) {
    LEAVE("bad source");
    return 0;
  }

  LEAVE("index is %d", source->index);
  return source->index;
}

gboolean
gnc_quote_source_get_supported (const gnc_quote_source *source)
{
  ENTER("%p", source);
  if (!source) {
    LEAVE("bad source");
    return FALSE;
  }

  LEAVE("%ssupported", source && source->supported ? "" : "not ");
  return source->supported;
}

const char *
gnc_quote_source_get_user_name (const gnc_quote_source *source)
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
gnc_quote_source_get_old_internal_name (const gnc_quote_source *source)
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
gnc_quote_source_get_internal_name (const gnc_quote_source *source)
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
gnc_quote_source_set_fq_installed (const GList *sources_list)
{
  gnc_quote_source *source;
  char *source_name;
  const GList *node;

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
 * QoF Helpers
 ********************************************************************/

void
gnc_commodity_begin_edit (gnc_commodity *cm)
{
  qof_begin_edit(&cm->inst);
}

static void commit_err (QofInstance *inst, QofBackendError errcode)
{
  PERR ("Failed to commit: %d", errcode);
}

static void noop (QofInstance *inst) {}

void
gnc_commodity_commit_edit (gnc_commodity *cm)
{
  if (!qof_commit_edit (QOF_INSTANCE(cm))) return;
  qof_commit_edit_part2 (&cm->inst, commit_err, noop, noop);
}

/********************************************************************
 * gnc_commodity_new
 ********************************************************************/

static void
mark_commodity_dirty (gnc_commodity *cm)
{
  qof_instance_set_dirty(&cm->inst);
  qof_event_gen (&cm->inst, QOF_EVENT_MODIFY, NULL);
}

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
    gnc_commodity_namespace *ns;

    g_free(com->unique_name);
    ns = com->namespace;
    com->unique_name = g_strdup_printf("%s::%s",
                                       ns ? ns->name : "",
                                       com->mnemonic ? com->mnemonic : "");
}

/* GObject Initialization */
QOF_GOBJECT_IMPL(gnc_commodity, gnc_commodity, QOF_TYPE_INSTANCE);

static void
gnc_commodity_init(gnc_commodity* com)
{
}

static void
gnc_commodity_dispose_real (GObject *nsp)
{
}

static void
gnc_commodity_finalize_real(GObject* comp)
{
}

gnc_commodity *
gnc_commodity_new(QofBook *book, const char * fullname, 
                  const char * namespace, const char * mnemonic, 
                  const char * cusip, int fraction)
{
  gnc_commodity * retval = g_object_new(GNC_TYPE_COMMODITY, NULL);
  gnc_commodity_table *table;

  qof_instance_init_data (&retval->inst, GNC_ID_COMMODITY, book);
  table = gnc_commodity_table_get_table(book);
  if (namespace) {
    retval->namespace = gnc_commodity_table_find_namespace(table, namespace);
    if (!retval->namespace)
      retval->namespace = gnc_commodity_table_add_namespace(table, namespace, book);
  } else {
    retval->namespace = NULL;
  }
  
  retval->fullname = CACHE_INSERT(fullname);
  retval->mnemonic = CACHE_INSERT(mnemonic);
  retval->cusip = CACHE_INSERT(cusip);
  retval->fraction = fraction;
  retval->mark = 0;
  retval->quote_flag = 0;
  retval->quote_source = NULL;
  retval->quote_tz = CACHE_INSERT("");

  reset_printname(retval);
  reset_unique_name(retval);
  if (gnc_commodity_namespace_is_iso(namespace))
    retval->quote_source = gnc_quote_source_lookup_by_internal("currency");
  qof_event_gen (&retval->inst, QOF_EVENT_CREATE, NULL);

  return retval;
}


/********************************************************************
 * gnc_commodity_destroy
 ********************************************************************/

void
gnc_commodity_destroy(gnc_commodity * cm)
{
  QofBook *book;
  gnc_commodity_table *table;
  if(!cm) return;

  book = qof_instance_get_book(&cm->inst);
  table = gnc_commodity_table_get_table(book);
  gnc_commodity_table_remove(table, cm);

  qof_event_gen (&cm->inst, QOF_EVENT_DESTROY, NULL);

  /* Set at creation */
  CACHE_REMOVE (cm->fullname);
  CACHE_REMOVE (cm->cusip);
  CACHE_REMOVE (cm->mnemonic);
  CACHE_REMOVE (cm->quote_tz);
  cm->namespace = NULL;

  /* Set through accessor functions */
  cm->quote_source = NULL;

  /* Automatically generated */
  g_free(cm->printname);
  cm->printname = NULL;

  g_free(cm->unique_name);
  cm->unique_name = NULL;

  cm->mark = 0;

#ifdef ACCOUNTS_CLEANED_UP
  /* Account objects are not actually cleaned up when a book is closed (in fact
   * a memory leak), but commodities are, so in currently this warning gets hit
   * quite frequently.  Disable the check until cleaning up of accounts objects
   * on close is implemented.  */
  if(cm->usage_count != 0) {
      PWARN("Destroying commodity (%p) with non-zero usage_count (%d).", cm,
              cm->usage_count);
  }
#endif

  /* qof_instance_release (&cm->inst); */
  g_object_unref(cm);
}

void
gnc_commodity_copy(gnc_commodity * dest, const gnc_commodity *src)
{
  gnc_commodity_set_fullname (dest, src->fullname);
  dest->namespace = src->namespace;
  gnc_commodity_set_fraction (dest, src->fraction);
  gnc_commodity_set_cusip (dest, src->cusip);
  gnc_commodity_set_quote_flag (dest, src->quote_flag);
  gnc_commodity_set_quote_source (dest, gnc_commodity_get_quote_source (src));
  gnc_commodity_set_quote_tz (dest, src->quote_tz);
  kvp_frame_delete (dest->inst.kvp_data);
  dest->inst.kvp_data = kvp_frame_copy (src->inst.kvp_data);
}

gnc_commodity *
gnc_commodity_clone(const gnc_commodity *src, QofBook *dest_book)
{
  gnc_commodity * dest = g_object_new(GNC_TYPE_COMMODITY, NULL);
  qof_instance_init_data (&dest->inst, GNC_ID_COMMODITY, dest_book);

  dest->fullname = CACHE_INSERT(src->fullname);
  dest->mnemonic = CACHE_INSERT(src->mnemonic);
  dest->cusip = CACHE_INSERT(src->cusip);
  dest->quote_tz = CACHE_INSERT(src->quote_tz);

  dest->namespace = src->namespace;

  dest->mark = 0;
  dest->fraction = src->fraction;
  dest->quote_flag = src->quote_flag;

  gnc_commodity_set_quote_source (dest, gnc_commodity_get_quote_source (src));

  kvp_frame_delete (dest->inst.kvp_data);
  dest->inst.kvp_data = kvp_frame_copy (src->inst.kvp_data);

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
  return gnc_commodity_namespace_get_name(cm->namespace);
}

const char *
gnc_commodity_get_namespace_compat(const gnc_commodity * cm) 
{
  if (!cm || !cm->namespace) return NULL;
  if (cm->namespace->iso4217) {
    /* Data files are still written with ISO4217. */
    return GNC_COMMODITY_NS_ISO;
  }
  return gnc_commodity_namespace_get_name(cm->namespace);
}

gnc_commodity_namespace *
gnc_commodity_get_namespace_ds(const gnc_commodity * cm) 
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
 * gnc_commodity_get_cusip
 ********************************************************************/

const char * 
gnc_commodity_get_cusip(const gnc_commodity * cm) 
{
  if(!cm) return NULL;
  return cm->cusip;
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
 * gnc_commodity_get_auto_quote_control_flag
 ********************************************************************/

static gboolean
gnc_commodity_get_auto_quote_control_flag(const gnc_commodity *cm)
{
  const char *str;

  if(!cm) return FALSE;

  str = kvp_frame_get_string(cm->inst.kvp_data, "auto_quote_control");
  return !str || (strcmp(str, "false") != 0);
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
  if(!cm) return;
  if(cm->mnemonic == mnemonic) return;

  gnc_commodity_begin_edit(cm);
  CACHE_REMOVE (cm->mnemonic);
  cm->mnemonic = CACHE_INSERT(mnemonic);

  mark_commodity_dirty (cm);
  reset_printname(cm);
  reset_unique_name(cm);
  gnc_commodity_commit_edit(cm);
}

/********************************************************************
 * gnc_commodity_set_namespace
 ********************************************************************/

void
gnc_commodity_set_namespace(gnc_commodity * cm, const char * namespace) 
{
  QofBook *book;
  gnc_commodity_table *table;
  gnc_commodity_namespace *nsp;

  if(!cm) return;
  book = qof_instance_get_book (&cm->inst);
  table = gnc_commodity_table_get_table(book);
  nsp = gnc_commodity_table_add_namespace(table, namespace, book);
  if (cm->namespace == nsp)
    return;

  gnc_commodity_begin_edit(cm);
  cm->namespace = nsp;
  if (nsp->iso4217)
    cm->quote_source = gnc_quote_source_lookup_by_internal("currency");
  mark_commodity_dirty(cm);
  reset_printname(cm);
  reset_unique_name(cm);
  gnc_commodity_commit_edit(cm);
}

/********************************************************************
 * gnc_commodity_set_fullname
 ********************************************************************/

void
gnc_commodity_set_fullname(gnc_commodity * cm, const char * fullname) 
{
  if(!cm) return;
  if(cm->fullname == fullname) return;

  CACHE_REMOVE (cm->fullname);
  cm->fullname = CACHE_INSERT (fullname);

  gnc_commodity_begin_edit(cm);
  mark_commodity_dirty(cm);
  reset_printname(cm);
  gnc_commodity_commit_edit(cm);
}

/********************************************************************
 * gnc_commodity_set_cusip
 ********************************************************************/

void
gnc_commodity_set_cusip(gnc_commodity * cm, 
			const char * cusip) 
{
  if(!cm) return;
  if(cm->cusip == cusip) return;

  gnc_commodity_begin_edit(cm);
  CACHE_REMOVE (cm->cusip);
  cm->cusip = CACHE_INSERT (cusip);
  mark_commodity_dirty(cm);
  gnc_commodity_commit_edit(cm);
}

/********************************************************************
 * gnc_commodity_set_fraction
 ********************************************************************/

void
gnc_commodity_set_fraction(gnc_commodity * cm, int fraction) 
{
  if(!cm) return;
  gnc_commodity_begin_edit(cm);
  cm->fraction = fraction;
  mark_commodity_dirty(cm);
  gnc_commodity_commit_edit(cm);
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
 * gnc_commodity_set_auto_quote_control_flag
 ********************************************************************/

static void
gnc_commodity_set_auto_quote_control_flag(gnc_commodity *cm,
                                          const gboolean flag)
{
  ENTER ("(cm=%p, flag=%d)", cm, flag);

  if(!cm) {
    LEAVE("");
    return;
  }

  gnc_commodity_begin_edit(cm);
  kvp_frame_set_string(cm->inst.kvp_data,
                       "auto_quote_control", flag ? NULL : "false");
  mark_commodity_dirty(cm);
  gnc_commodity_commit_edit(cm);
  LEAVE("");
}

/********************************************************************
 * gnc_commodity_user_set_quote_flag
 ********************************************************************/

void
gnc_commodity_user_set_quote_flag(gnc_commodity *cm, const gboolean flag)
{
  ENTER ("(cm=%p, flag=%d)", cm, flag);

  if(!cm) {
    LEAVE("");
    return;
  }

  gnc_commodity_begin_edit(cm);
  gnc_commodity_set_quote_flag(cm, flag);
  if(gnc_commodity_is_iso(cm)) {
    /* For currencies, disable auto quote control if the quote flag is being
     * changed from its default value and enable it if the quote flag is being
     * reset to its default value.  The defaults for the quote flag are
     * disabled if no accounts are using the currency, and true otherwise.
     * Thus enable auto quote control if flag is FALSE and there are not any
     * accounts using this currency OR flag is TRUE and there are accounts
     * using this currency; otherwise disable auto quote control */
    gnc_commodity_set_auto_quote_control_flag(cm,
        (!flag && (cm->usage_count == 0)) || (flag && (cm->usage_count != 0)));
  }
  gnc_commodity_commit_edit(cm);
  LEAVE("");
}

/********************************************************************
 * gnc_commodity_set_quote_flag
 ********************************************************************/

void
gnc_commodity_set_quote_flag(gnc_commodity *cm, const gboolean flag)
{
  ENTER ("(cm=%p, flag=%d)", cm, flag);

  if(!cm) return;
  gnc_commodity_begin_edit(cm);
  cm->quote_flag = flag;
  mark_commodity_dirty(cm);
  gnc_commodity_commit_edit(cm);
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
  gnc_commodity_begin_edit(cm);
  cm->quote_source = src;
  mark_commodity_dirty(cm);
  gnc_commodity_commit_edit(cm);
  LEAVE(" ");
}

/********************************************************************
 * gnc_commodity_set_quote_tz
 ********************************************************************/

void
gnc_commodity_set_quote_tz(gnc_commodity *cm, const char *tz) 
{
  ENTER ("(cm=%p, tz=%s)", cm, tz ? tz : "(null)");

  if(!cm || tz == cm->quote_tz) return;

  gnc_commodity_begin_edit(cm);
  CACHE_REMOVE (cm->quote_tz);
  cm->quote_tz = CACHE_INSERT (tz);
  mark_commodity_dirty(cm);
  gnc_commodity_commit_edit(cm);
  LEAVE(" ");
}

/********************************************************************
 * gnc_commodity_increment_usage_count
 ********************************************************************/

void
gnc_commodity_increment_usage_count(gnc_commodity *cm)
{
  const char *str;

  ENTER("(cm=%p)", cm);

  if(!cm) {
    LEAVE("");
    return;
  }

  if((cm->usage_count == 0) && !cm->quote_flag
      && gnc_commodity_get_auto_quote_control_flag(cm)
      && gnc_commodity_is_iso(cm)) {
    /* compatability hack - Gnucash 1.8 gets currency quotes when a
       non-default currency is assigned to an account.  */
    gnc_commodity_begin_edit(cm);
    gnc_commodity_set_quote_flag(cm, TRUE);
    gnc_commodity_set_quote_source(cm,
        gnc_commodity_get_default_quote_source(cm));
    gnc_commodity_commit_edit(cm);
  }
  cm->usage_count++;
  LEAVE("(usage_count=%d)", cm->usage_count);
}

/********************************************************************
 * gnc_commodity_decrement_usage_count
 ********************************************************************/

void
gnc_commodity_decrement_usage_count(gnc_commodity *cm)
{
  const char *str;

  ENTER("(cm=%p)", cm);

  if(!cm) {
    LEAVE("");
    return;
  }

  if(cm->usage_count == 0) {
    PWARN("usage_count already zero");
    LEAVE("");
    return;
  }

  cm->usage_count--;
  if((cm->usage_count == 0) && cm->quote_flag
      && gnc_commodity_get_auto_quote_control_flag(cm)
      && gnc_commodity_is_iso(cm)) {
    /* if this is a currency with auto quote control enabled and no more
     * accounts reference this currency, disable quote retrieval */
    gnc_commodity_set_quote_flag(cm, FALSE);
  }
  LEAVE("(usage_count=%d)", cm->usage_count);
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
  if(a->namespace != b->namespace) return FALSE;
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

  if (a->namespace != b->namespace)
  {
    DEBUG ("namespaces differ: %p(%s) vs %p(%s)",
	   a->namespace, gnc_commodity_namespace_get_name(a->namespace),
	   b->namespace, gnc_commodity_namespace_get_name(b->namespace));
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

  if (safe_strcmp(a->cusip, b->cusip) != 0)
  {
    DEBUG ("cusips differ: %s vs %s", a->cusip, b->cusip);
    return FALSE;
  }

  if (a->fraction != b->fraction)
  {
    DEBUG ("fractions differ: %d vs %d", a->fraction, b->fraction);
    return FALSE;
  }

  return TRUE;
}


/************************************************************
 *                   Namespace functions                    *
 ************************************************************/
const char *
gnc_commodity_namespace_get_name (const gnc_commodity_namespace *ns)
{
  if (ns == NULL)
    return NULL;
  return ns->name;
}

GList * 
gnc_commodity_namespace_get_commodity_list(const gnc_commodity_namespace *namespace) 
{
  if (!namespace)
    return NULL;

  return namespace->cm_list;
}

gboolean
gnc_commodity_namespace_is_iso(const char *namespace)
{
  return ((safe_strcmp(namespace, GNC_COMMODITY_NS_ISO) == 0) ||
	  (safe_strcmp(namespace, GNC_COMMODITY_NS_CURRENCY) == 0));
}

static const gchar *
gnc_commodity_table_map_namespace(const char * namespace)
{
  if (safe_strcmp(namespace, GNC_COMMODITY_NS_ISO) == 0)
    return GNC_COMMODITY_NS_CURRENCY;
  return namespace;
}

/********************************************************************
 * gnc_commodity_table_new
 * make a new commodity table 
 ********************************************************************/

gnc_commodity_table *
gnc_commodity_table_new(void) 
{
  gnc_commodity_table * retval = g_new0(gnc_commodity_table, 1);
  retval->ns_table = g_hash_table_new(&g_str_hash, &g_str_equal);
  retval->ns_list = NULL;
  return retval;
}

/********************************************************************
 * book anchor functons
 ********************************************************************/

gnc_commodity_table *
gnc_commodity_table_get_table(QofBook *book)
{
  if (!book) return NULL;
  return qof_book_get_data (book, GNC_COMMODITY_TABLE);
}

gnc_commodity *
gnc_commodity_obtain_twin (const gnc_commodity *from, QofBook *book)
{
  gnc_commodity *twin;
  const char * ucom;
  gnc_commodity_table * comtbl;

  if (!from) return NULL;
  comtbl = gnc_commodity_table_get_table (book);
  if (!comtbl) return NULL;

  ucom = gnc_commodity_get_unique_name (from);
  twin = gnc_commodity_table_lookup_unique (comtbl, ucom);
  if (!twin)
  {
    twin = gnc_commodity_clone (from, book);
    twin = gnc_commodity_table_insert (comtbl, twin);
  }
  return twin;
}

/********************************************************************
 * gnc_commodity_table_get_size
 * get the size of the commodity table
 ********************************************************************/

guint
gnc_commodity_table_get_number_of_namespaces(const gnc_commodity_table* tbl)
{
    g_return_val_if_fail(tbl, 0);
    g_return_val_if_fail(tbl->ns_table, 0);
    return g_hash_table_size(tbl->ns_table);
}

static void
count_coms(gpointer key, gpointer value, gpointer user_data)
{
    GHashTable *tbl = ((gnc_commodity_namespace*)value)->cm_table;
    guint *count = (guint*)user_data;

    if(safe_strcmp((char*)key, GNC_COMMODITY_NS_CURRENCY) == 0)
    {
        /* don't count default commodities */
        return;
    }
    
    if(!value) return;
    
    *count += g_hash_table_size(tbl);
}

guint
gnc_commodity_table_get_size(const gnc_commodity_table* tbl)
{
    guint count = 0;
    g_return_val_if_fail(tbl, 0);
    g_return_val_if_fail(tbl->ns_table, 0);

    g_hash_table_foreach(tbl->ns_table, count_coms, (gpointer)&count);

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

  nsp = gnc_commodity_table_find_namespace(table, namespace);

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
    return g_hash_table_lookup(nsp->cm_table, (gpointer)mnemonic);
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
  gnc_commodity_namespace * nsp = NULL;
  gnc_commodity *c;
  const char *ns_name;
  QofBook *book;

  if (!table) return NULL;
  if (!comm) return NULL;

  ENTER ("(table=%p, comm=%p) %s %s", table, comm,
         (comm->mnemonic == NULL ? "(null)" : comm->mnemonic),
         (comm->fullname == NULL ? "(null)" : comm->fullname));
  ns_name = gnc_commodity_namespace_get_name(comm->namespace);
  c = gnc_commodity_table_lookup (table, ns_name, comm->mnemonic);

  if (c) 
  {
    if (c == comm)
    {
      LEAVE("already in table");
      return c;
    }
    gnc_commodity_copy (c, comm);
    gnc_commodity_destroy (comm);
    LEAVE("found at %p", c);
    return c;
  }

  book = qof_instance_get_book (&comm->inst);
  nsp = gnc_commodity_table_add_namespace(table, ns_name, book);

  PINFO ("insert %p %s into nsp=%p %s", comm->mnemonic, comm->mnemonic,
         nsp->cm_table, nsp->name);
  g_hash_table_insert(nsp->cm_table, 
                      CACHE_INSERT(comm->mnemonic),
                      (gpointer)comm);
  nsp->cm_list = g_list_append(nsp->cm_list, comm);

  qof_event_gen (&comm->inst, QOF_EVENT_ADD, NULL);
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
  const char *ns_name;

  if (!table) return;
  if (!comm) return;

  ns_name = gnc_commodity_namespace_get_name(comm->namespace);
  c = gnc_commodity_table_lookup (table, ns_name, comm->mnemonic);
  if (c != comm) return;

  qof_event_gen (&comm->inst, QOF_EVENT_REMOVE, NULL);

  nsp = gnc_commodity_table_find_namespace(table, ns_name);
  if (!nsp) return;

  nsp->cm_list = g_list_remove(nsp->cm_list, comm);
  g_hash_table_remove (nsp->cm_table, comm->mnemonic);
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

  nsp = gnc_commodity_table_find_namespace(table, namespace);
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

  return g_hash_table_keys(table->ns_table);
}

GList * 
gnc_commodity_table_get_namespaces_list(const gnc_commodity_table * table) 
{
  if (!table)
    return NULL;

  return table->ns_list;
}

gboolean
gnc_commodity_is_iso(const gnc_commodity * cm)
{
  if (!cm || !cm->namespace) return FALSE;
  return cm->namespace->iso4217;
}

gboolean
gnc_commodity_is_currency(const gnc_commodity *cm)
{
    const char *ns_name;
    if (!cm) return FALSE;

    ns_name = gnc_commodity_namespace_get_name(cm->namespace);
    return (!safe_strcmp(ns_name, GNC_COMMODITY_NS_LEGACY) ||
            !safe_strcmp(ns_name, GNC_COMMODITY_NS_CURRENCY));
}

/********************************************************************
 * gnc_commodity_table_get_commodities
 * list commodities in a give namespace 
 ********************************************************************/

CommodityList *
gnc_commodity_table_get_commodities(const gnc_commodity_table * table,
                                    const char * namespace) 
{
  gnc_commodity_namespace * ns = NULL; 

  if (!table)
    return NULL;

  ns = gnc_commodity_table_find_namespace(table, namespace);
  if (!ns)
    return NULL;

  return g_hash_table_values(ns->cm_table);
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

CommodityList *
gnc_commodity_table_get_quotable_commodities(const gnc_commodity_table * table)
{
  gnc_commodity_namespace * ns = NULL;
  const char *namespace;
  GList * nslist, * tmp;
  GList * l = NULL;
  regex_t pattern;
  const char *expression = gnc_main_get_namespace_regexp();

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
	ns = gnc_commodity_table_find_namespace(table, namespace);
	if (ns) {
	  g_hash_table_foreach(ns->cm_table, &get_quotables_helper1, (gpointer) &l);
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

/* GObject Initialization */
QOF_GOBJECT_IMPL(gnc_commodity_namespace, gnc_commodity_namespace, QOF_TYPE_INSTANCE);

static void
gnc_commodity_namespace_init(gnc_commodity_namespace* ns)
{
}

static void
gnc_commodity_namespace_dispose_real (GObject *nsp)
{
}

static void
gnc_commodity_namespace_finalize_real(GObject* nsp)
{
}

gnc_commodity_namespace * 
gnc_commodity_table_add_namespace(gnc_commodity_table * table,
                                  const char * namespace,
				  QofBook *book) 
{
  gnc_commodity_namespace * ns = NULL; 
  
  if (!table) return NULL;
  
  namespace = gnc_commodity_table_map_namespace(namespace);
  ns = gnc_commodity_table_find_namespace(table, namespace);
  if(!ns) 
  {
    ns = g_object_new(GNC_TYPE_COMMODITY_NAMESPACE, NULL);
    ns->cm_table = g_hash_table_new(g_str_hash, g_str_equal);
    ns->name = CACHE_INSERT((gpointer)namespace);
    ns->iso4217 = gnc_commodity_namespace_is_iso(namespace);
    qof_instance_init_data (&ns->inst, GNC_ID_COMMODITY_NAMESPACE, book);
    qof_event_gen (&ns->inst, QOF_EVENT_CREATE, NULL);
    
    g_hash_table_insert(table->ns_table,
			(gpointer) ns->name, 
			(gpointer) ns);
    table->ns_list = g_list_append(table->ns_list, ns);
    qof_event_gen (&ns->inst, QOF_EVENT_ADD, NULL);
  }
  return ns;
}


gnc_commodity_namespace * 
gnc_commodity_table_find_namespace(const gnc_commodity_table * table,
				   const char * namespace) 
{
  if (!table || !namespace)
    return NULL;

  namespace = gnc_commodity_table_map_namespace(namespace);
  return g_hash_table_lookup(table->ns_table, (gpointer)namespace);
}


gnc_commodity *
gnc_commodity_find_commodity_by_guid(const GUID *guid, QofBook *book)
{
  QofCollection *col;
  if (!guid || !book) return NULL;
  col = qof_book_get_collection (book, GNC_ID_COMMODITY);
  return (gnc_commodity *) qof_collection_lookup_entity (col, guid);
}

gnc_commodity_namespace *
gnc_commodity_find_namespace_by_guid(const GUID *guid, QofBook *book)
{
  QofCollection *col;
  if (!guid || !book) return NULL;
  col = qof_book_get_collection (book, GNC_ID_COMMODITY_NAMESPACE);
  return (gnc_commodity_namespace *) qof_collection_lookup_entity (col, guid);
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
  CACHE_REMOVE(key);  /* key is commodity mnemonic */
  return TRUE;
}

void 
gnc_commodity_table_delete_namespace(gnc_commodity_table * table,
                                     const char * namespace) 
{
  gnc_commodity_namespace * ns;

  if (!table) return;

  ns = gnc_commodity_table_find_namespace(table, namespace);
  if (!ns)
    return;

  qof_event_gen (&ns->inst, QOF_EVENT_REMOVE, NULL);
  g_hash_table_remove(table->ns_table, namespace);
  table->ns_list = g_list_remove(table->ns_list, ns);

  g_list_free(ns->cm_list);
  ns->cm_list = NULL;

  g_hash_table_foreach_remove(ns->cm_table, ns_helper, NULL);
  g_hash_table_destroy(ns->cm_table);
  CACHE_REMOVE(ns->name);

  qof_event_gen (&ns->inst, QOF_EVENT_DESTROY, NULL);
  /* qof_instance_release(&ns->inst); */
  g_object_unref(ns);
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
  GHashTable *namespace_hash = ((gnc_commodity_namespace *) value)->cm_table;
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

  g_hash_table_foreach(tbl->ns_table, iter_namespace, (gpointer)&iter_data);

  return iter_data.ok;
}

/********************************************************************
 * gnc_commodity_table_destroy
 * cleanup and free. 
 ********************************************************************/

void
gnc_commodity_table_destroy(gnc_commodity_table * t) 
{
  gnc_commodity_namespace * ns;
  GList *item, *next;

  if (!t) return;
  ENTER ("table=%p", t);
  
  for (item = t->ns_list; item; item = next) {
    next = g_list_next(item);
    ns = item->data;
    gnc_commodity_table_delete_namespace(t, ns->name);
  }

  g_list_free(t->ns_list);
  t->ns_list = NULL;
  g_hash_table_destroy(t->ns_table);
  t->ns_table = NULL;
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

typedef struct {
  gnc_commodity_table *dest;
  QofBook *dest_book;
} table_copy_helper_data;

static gboolean 
table_copy_helper (gnc_commodity *src_cm, gpointer user_data)
{
  table_copy_helper_data *data = user_data;
  gnc_commodity_table_insert (data->dest,
      gnc_commodity_clone (src_cm, data->dest_book));
  return TRUE;
}

void
gnc_commodity_table_copy(gnc_commodity_table *dest,
                          gnc_commodity_table *src,
                          QofBook *dest_book)
{
  table_copy_helper_data data = {dest, dest_book};
  gnc_commodity_table_foreach_commodity (src, table_copy_helper, &data);
}

/********************************************************************
 * gnc_commodity_table_add_default_data
 ********************************************************************/

gboolean
gnc_commodity_table_add_default_data(gnc_commodity_table *table, QofBook *book)
{
  QofCollection *col;
  gnc_commodity* c;

  ENTER ("table=%p", table);
  gnc_commodity_table_add_namespace(table, GNC_COMMODITY_NS_AMEX, book);
  gnc_commodity_table_add_namespace(table, GNC_COMMODITY_NS_NYSE, book);
  gnc_commodity_table_add_namespace(table, GNC_COMMODITY_NS_NASDAQ, book);
  gnc_commodity_table_add_namespace(table, GNC_COMMODITY_NS_EUREX, book);
  gnc_commodity_table_add_namespace(table, GNC_COMMODITY_NS_MUTUAL, book);
  gnc_commodity_table_add_namespace(table, "template", book);
  c = gnc_commodity_new(book, "template", "template", "template", "template", 1);
  gnc_commodity_table_insert(table, c);

  #include "iso-4217-currencies.c"

  /* We've just created the default namespaces and currencies.  Mark
   * these collections as clean because there is no USER entered data
   * in these collections as of yet. */
  col = qof_book_get_collection(book, GNC_ID_COMMODITY);
  qof_collection_mark_clean(col);
  col = qof_book_get_collection(book, GNC_ID_COMMODITY_NAMESPACE);
  qof_collection_mark_clean(col);

  LEAVE ("table=%p", table);
  return TRUE;
}

/********************************************************************
 ********************************************************************/
/* QofObject function implementation and registration */

static QofObject commodity_object_def = 
{
  interface_version: QOF_OBJECT_VERSION,
  e_type:            GNC_ID_COMMODITY,
  type_label:        "Commodity",
  book_begin:        NULL,
  book_end:          NULL,
  is_dirty:          qof_collection_is_dirty,
  mark_clean:        qof_collection_mark_clean,
  foreach:           qof_collection_foreach,
  printable:         (const char* (*)(gpointer)) gnc_commodity_get_fullname,
};

static QofObject namespace_object_def = 
{
  interface_version: QOF_OBJECT_VERSION,
  e_type:            GNC_ID_COMMODITY_NAMESPACE,
  type_label:        "Namespace",
  book_begin:        NULL,
  book_end:          NULL,
  is_dirty:          NULL,
  mark_clean:        NULL,
  foreach:           NULL,
  printable:         NULL,
};

static void 
commodity_table_book_begin (QofBook *book)
{
  gnc_commodity_table *ct;
  ENTER ("book=%p", book);
  
  if (gnc_commodity_table_get_table(book))
    return;

  ct = gnc_commodity_table_new ();
  qof_book_set_data (book, GNC_COMMODITY_TABLE, ct);

  if(!gnc_commodity_table_add_default_data(ct, book))
  {
    PWARN("unable to initialize book's commodity_table");
  }

  LEAVE ("book=%p", book);
}

static void 
commodity_table_book_end (QofBook *book)
{
  gnc_commodity_table *ct;

  ct = gnc_commodity_table_get_table (book);
  qof_book_set_data (book, GNC_COMMODITY_TABLE, NULL);
  gnc_commodity_table_destroy (ct);
}

static QofObject commodity_table_object_def = 
{
  interface_version: QOF_OBJECT_VERSION,
  e_type:            GNC_ID_COMMODITY_TABLE,
  type_label:        "CommodityTable",
  create:            NULL,
  book_begin:        commodity_table_book_begin,
  book_end:          commodity_table_book_end,
  is_dirty:          qof_collection_is_dirty,
  mark_clean:        qof_collection_mark_clean,
  foreach:           NULL,
  printable:         NULL,
  version_cmp:       NULL,
};

gboolean 
gnc_commodity_table_register (void)
{
  gnc_quote_source_init_tables();
  
  if (!qof_object_register (&commodity_object_def))
    return FALSE;
  if (!qof_object_register (&namespace_object_def))
    return FALSE;
  return qof_object_register (&commodity_table_object_def);
}

/* ========================= END OF FILE ============================== */
