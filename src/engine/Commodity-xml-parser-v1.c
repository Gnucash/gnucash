#include "config.h"

#include <glib.h>
#include <string.h>

#include "sixtp.h"
#include "sixtp-utils.h"
#include "sixtp-parsers.h"

#include "gnc-commodity.h"
#include "gnc-engine.h"

/****************************************************************************/
/* Commodity restorer.

   Right now we just check to see that fields aren't duplicated.  If
   fields don't show up, then we just use "".

   We also check to see that we get a <fraction>.  If not, it's an
   error.

   Example:   
     <commodity>
       <restore>
         <space>NASDAQ</space>
         <id>XYZZY</id>
         <name>Grue Enterprises</name>
         <xcode>XXX</xcode>
         <fraction>100</fraction>
       </restore>
     </commodity>

 */

/* ==================================================================== */

/*********************************/
/* <restore> (lineage <commodity>)

   Start handler allocates a gnc_commodity.  The end_handler, if
   everything's OK, crams the commodity into the engine, otherwise it
   deletes it.

   input: NA
   returns: NA

   start: allocate CommodityParseInfo* and put it into data_for_children.
   characters: allow and ignore only whitespace.
   after-child: handle strings from simple chars children.
   end: if OK create gnc_commodity and add to engine.  delete CommodityParseInfo.

   cleanup-result: NA
   cleanup-chars: NA
   fail: delete CommodityParseInfo*.
   result-fail: NA
   chars-fail: NA

 */

typedef struct {
  gchar *space;
  gchar *id;
  gchar *name;
  gchar *xcode;
  gboolean seen_fraction;
  int fraction;
} CommodityParseInfo;

static gboolean
commodity_restore_start_handler(GSList* sibling_data, gpointer parent_data,
                                gpointer global_data,
                                gpointer *data_for_children, gpointer *result,
                                const gchar *tag, gchar **attrs)
{
  CommodityParseInfo *cpi = (CommodityParseInfo *) g_new0(CommodityParseInfo, 1);

  g_return_val_if_fail(cpi, FALSE);

  *data_for_children = cpi;
  return(TRUE);
}

/* ----------------------------------------------------*/
#define COMMOD_TOKEN(NAME)				\
  if(strcmp(child_result->tag, #NAME) == 0) {		\
    if(cpi->NAME) return(FALSE);			\
    cpi->NAME = (gchar *) child_result->data;		\
    child_result->should_cleanup = FALSE;		\
  }							\
  else 
/* ----------------------------------------------------*/

static gboolean
commodity_restore_after_child_handler(gpointer data_for_children,
                           GSList* data_from_children,
                           GSList* sibling_data,
                           gpointer parent_data,
                           gpointer global_data,
                           gpointer *result,
                           const gchar *tag,
                           const gchar *child_tag,
                           sixtp_child_result *child_result)
{
  CommodityParseInfo *cpi = (CommodityParseInfo *) data_for_children;

  g_return_val_if_fail(cpi, FALSE);
  g_return_val_if_fail(child_result, FALSE);

  COMMOD_TOKEN(space)
  COMMOD_TOKEN(id)
  COMMOD_TOKEN(name)
  COMMOD_TOKEN(xcode)
  if(strcmp(child_result->tag, "fraction") == 0) {
    gint64 frac;
    gboolean conv_ok;

    if(cpi->seen_fraction) return(FALSE);
    conv_ok = string_to_gint64((gchar *) child_result->data, &frac);
    cpi->fraction = frac;
    cpi->seen_fraction = TRUE;
    child_result->should_cleanup = TRUE;
  } else {
    /* redundant because the parser won't allow any other children */
    return(FALSE);
  }

  return(TRUE);
}

static gboolean
commodity_restore_end_handler(gpointer data_for_children,
                              GSList  *data_from_children, GSList *sibling_data,
                              gpointer parent_data, gpointer global_data,
                              gpointer *result, const gchar *tag)
{
  CommodityParseInfo *cpi = (CommodityParseInfo *) data_for_children;
  gboolean ok = FALSE;
  gnc_commodity *comm = NULL;

  g_return_val_if_fail(cpi, FALSE);

  if(cpi->seen_fraction) {
    gnc_commodity *comm;

    if(!cpi->space) cpi->space = g_strdup("");
    if(!cpi->id) cpi->id = g_strdup("");
    if(!cpi->name) cpi->name = g_strdup("");
    if(!cpi->xcode) cpi->xcode = g_strdup("");

    comm = gnc_commodity_new(cpi->name,
                             cpi->space,
                             cpi->id,
                             cpi->xcode,
                             cpi->fraction);
    if(comm) {
      gnc_commodity_table *ctab = gnc_engine_commodities();
      if(ctab) {
        gnc_commodity_table_insert(ctab, comm);
        ok = TRUE;
      }
    }
  }

  g_free(cpi->space);
  g_free(cpi->id);
  g_free(cpi->name);
  g_free(cpi->xcode);
  g_free(cpi);

  if(!ok) g_free(comm);

  return(ok);
}


sixtp *
commodity_restore_parser_new(void) 
{
  sixtp *top_level;
  sixtp *restore_pr;

  top_level = sixtp_new();
  g_return_val_if_fail(top_level, NULL);

  if(!(restore_pr = sixtp_set_any(
           sixtp_new(), FALSE,
           SIXTP_START_HANDLER_ID, commodity_restore_start_handler,
           SIXTP_END_HANDLER_ID, commodity_restore_end_handler,
           SIXTP_FAIL_HANDLER_ID, generic_free_data_for_children,
           SIXTP_AFTER_CHILD_HANDLER_ID, commodity_restore_after_child_handler,
           SIXTP_NO_MORE_HANDLERS)))
  {
      sixtp_destroy(top_level);
      return(NULL);
  }
  sixtp_add_sub_parser(top_level, "restore", restore_pr);
  
  if(!sixtp_add_some_sub_parsers(
         restore_pr, TRUE,
         "space", simple_chars_only_parser_new(NULL),
         "id", simple_chars_only_parser_new(NULL),
         "name", simple_chars_only_parser_new(NULL),
         "xcode", simple_chars_only_parser_new(NULL),
         "fraction", simple_chars_only_parser_new(NULL),
         0))
  {
      return NULL;
  }

  return(top_level);
}

/****************************************************************************/
/* generic gnc_commodity lookup handler.
   
   A collection of node functions intended to parse a sub-node set
   that looks like this:

     <security>
       <space>NASDAQ</space>
       <id>ZXDDQ</id>
     </security>

   and produce a gnc_commodity* by looking up the unique combination
   of namespace and ID (mnemonic).

   The start handler for the top allocates a CommodityParseInfo* and
   passes it to the children.  The <space> block sets the namespace
   and the <id> block sets the ID.  The end handler performs the
   lookup.  If all goes well, returns the gnc_commodity* as the
   result.  */

/* Top level gnc_commodity lookup node:

   input: NA
   returns: gnc_commodity*

   start: Allocates CommodityParseInfo* for data_for_children.
   characters: none (whitespace only).
   end: lookup commodity and place into *result, free data_for_children.

   fail: g_free data_for_children (CommodityParseInfo and contents).
   cleanup-chars: NA
   chars-fail: NA
   cleanup-result: NA (we didn't create the gnc_commodity we're returning)
   result-fail: NA

 */

typedef struct {
  gchar *namespace;
  gchar *id;
} CommodityLookupParseInfo;

static gboolean
generic_gnc_commodity_lookup_start_handler(
    GSList* sibling_data, gpointer parent_data, gpointer global_data,
    gpointer *data_for_children, gpointer *result, const gchar *tag,
    gchar **attrs)
{
  CommodityLookupParseInfo *cpi = g_new0(CommodityLookupParseInfo, 1);
  g_return_val_if_fail(cpi, FALSE);
  *data_for_children = cpi;
  return(TRUE);
}

static gboolean
generic_gnc_commodity_lookup_after_child_handler(gpointer data_for_children,
                           GSList* data_from_children,
                           GSList* sibling_data,
                           gpointer parent_data,
                           gpointer global_data,
                           gpointer *result,
                           const gchar *tag,
                           const gchar *child_tag,
                           sixtp_child_result *child_result)
{
  CommodityLookupParseInfo *cpi =
  (CommodityLookupParseInfo *) data_for_children;

  g_return_val_if_fail(cpi, FALSE);
  g_return_val_if_fail(child_result, FALSE);
  if(child_result->type != SIXTP_CHILD_RESULT_NODE) return(FALSE);

  if(strcmp(child_result->tag, "space") == 0) {
    if(cpi->namespace) return(FALSE);
    cpi->namespace = (gchar *) child_result->data;
    child_result->should_cleanup = FALSE;
  }
  else if(strcmp(child_result->tag, "id") == 0) {
    if(cpi->id) return(FALSE);
    cpi->id = (gchar *) child_result->data;
    child_result->should_cleanup = FALSE;
  } else {
    /* redundant because the parser won't allow any other children */
    return(FALSE);
  }

  return(TRUE);
}

static gboolean
generic_gnc_commodity_lookup_end_handler(gpointer data_for_children,
                                         GSList  *data_from_children, GSList *sibling_data,
                                         gpointer parent_data, gpointer global_data,
                                         gpointer *result, const gchar *tag)
{
  CommodityLookupParseInfo *cpi =
  (CommodityLookupParseInfo *) data_for_children;
  gboolean ok = FALSE;

  g_return_val_if_fail(cpi, FALSE);

  if(cpi->namespace && cpi->id) {
    gnc_commodity *com =
      gnc_commodity_table_lookup(gnc_engine_commodities(),
                                 cpi->namespace,
                                 cpi->id);
    if(com) {
      *result = com;
      ok = TRUE;
    }
  }

  g_free(cpi->namespace);
  g_free(cpi->id);
  g_free(cpi);
  return(ok);
}


sixtp *
generic_gnc_commodity_lookup_parser_new(void) 
{
  sixtp *top_level;

  if(!(top_level = sixtp_set_any(
           sixtp_new(), FALSE,
           SIXTP_START_HANDLER_ID, generic_gnc_commodity_lookup_start_handler,
           SIXTP_CHARACTERS_HANDLER_ID, allow_and_ignore_only_whitespace,
           SIXTP_END_HANDLER_ID, generic_gnc_commodity_lookup_end_handler,
           SIXTP_FAIL_HANDLER_ID, generic_free_data_for_children,
           SIXTP_AFTER_CHILD_HANDLER_ID,
           generic_gnc_commodity_lookup_after_child_handler,
           SIXTP_NO_MORE_HANDLERS)))
  {
      return NULL;
  }

  if(!sixtp_add_some_sub_parsers(
         top_level, TRUE,
         "space", simple_chars_only_parser_new(NULL),
         "id", simple_chars_only_parser_new(NULL),
         0))
  {
      return NULL;
  }

  return(top_level);
}
