/********************************************************************
 * io-gncxml-r.c -- read XML-format gnucash data file               *
 * Copyright (C) 2000 Gnumatic, Inc.                                *
 *                                                                  *
 * Initial code by Rob L. Browning 4Q 2000                          *
 * Tuneups by James LewisMoss Dec 2000                              *
 * Excessive hacking Linas Vepstas January 2001                     *
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

#include <stdlib.h>
#include <string.h>

#include <glib.h>

#include "gnc-xml-helper.h"
#include "Account.h"
#include "AccountP.h"
#include "Query.h"
#include "QueryP.h"
#include "Scrub.h"
#include "Transaction.h"
#include "TransactionP.h"
#include "TransLog.h"
#include "gnc-pricedb.h"
#include "gnc-pricedb-p.h"
#include "io-gncxml.h"

#include "sixtp.h"
#include "sixtp-dom-parsers.h"
#include "sixtp-stack.h"
#include "sixtp-parsers.h"
#include "sixtp-utils.h"

/* from Transaction-xml-parser-v1.c */
static sixtp* gnc_transaction_parser_new(void);

/* from Account-xml-parser-v1.c */
static sixtp* gnc_account_parser_new(void);

/* from Ledger-xml-parser-v1.c */
static sixtp* ledger_data_parser_new(void);

/* from Commodity-xml-parser-v1.c */
static sixtp* commodity_restore_parser_new(void);

/* from Commodity-xml-parser-v1.c */
static sixtp* generic_gnc_commodity_lookup_parser_new(void);

/* from Query-xml-parser-v1.c */
//static sixtp* query_server_parser_new (void);

/* from sixtp-kvp-parser.c */
static sixtp* kvp_frame_parser_new(void);

/* from gnc-pricedb-xml-v1.c */
static sixtp* gnc_pricedb_parser_new(void);


typedef enum {
  GNC_PARSE_ERR_NONE,
  GNC_PARSE_ERR_BAD_VERSION,
} GNCParseErr;

typedef struct {
  /* have we gotten the file version yet? */
  gboolean seen_version;
  gint64 version;

  /* top level <gnc-data> parser - we need this so we can set it up
     after we see the file version. */
  sixtp *gnc_parser;

  /* The book */
  QofBook *book;

  /* The root account */
  Account *root_account;

  /* The pricedb */
  GNCPriceDB *pricedb;

  /* The query */
  //  Query *query;

  GNCParseErr error;
} GNCParseStatus;


/* result for a characters handler is shared across all character
   handlers for a given node.  result for start/end pair is shared as
   well. 

   Cleaning up the results returned by children and characters
   handlers is the user's responsibility.

   results have to have cleanup pointers for exceptional failures

   stack frames also have to have cleanup funcs for exceptional
   failures after the start tag handler has been called, but before
   the end tag handler has been called.  If the end tag handler is
   called, but returns FALSE, it is expected that the end tag handler
   has taken care of any cleanup itself.

   result cleanup functions are called for a node's children just
   after the end handler unless should_cleanup has been set to FALSE,
   or unless there's a failure.  If there's a failure, then the
   cleanup is left to the failure handler.


*/

static QofLogModule log_module = GNC_MOD_IO;

/* ================================================================= */
/* <version> (lineage <gnc>)
   
   Fancy and strange - look for an integer version number.  If we get
   one, then modify the parent parser to handle the input.

   this is a simple_chars_only_parser with an end handler that grabs
   the version number and tweaks the parser, if possible.

 */

static gboolean
gnc_parser_configure_for_input_version(GNCParseStatus *status, gint64 version) 
{

  status->version = version;
  status->seen_version = TRUE;

  /* Check for a legal version here. */
  if(version != 1) {
    status->error = GNC_PARSE_ERR_BAD_VERSION;
    return(FALSE);
  }
  
  /* Now set up the parser based on the version. */
  
  /* add <ledger-data> */
  {
    sixtp *ledger_data_pr = ledger_data_parser_new();
    g_return_val_if_fail(ledger_data_pr, FALSE);
    sixtp_add_sub_parser(status->gnc_parser, "ledger-data", ledger_data_pr);
  }

#if 0
  /* add <query-server> */
  {
    sixtp *query_server_pr = query_server_parser_new();
    g_return_val_if_fail(query_server_pr, FALSE);
    sixtp_add_sub_parser(status->gnc_parser, "query-server", query_server_pr);
  }
#endif

  return(TRUE);
}

static gboolean
gnc_version_end_handler(gpointer data_for_children,
                        GSList  *data_from_children, GSList *sibling_data,
                        gpointer parent_data, gpointer global_data,
                        gpointer *result, const gchar *tag)
{
  GNCParseStatus *pstatus = (GNCParseStatus *) global_data;
  gint64 version;
  gboolean ok;
  gchar *txt;
  
  g_return_val_if_fail(pstatus, FALSE);
  if(pstatus->seen_version) return(FALSE);
  
  txt = concatenate_child_result_chars(data_from_children);
  g_return_val_if_fail(txt, FALSE);
  
  ok = string_to_gint64(txt, &version);
  g_free(txt);
  g_return_val_if_fail(ok, FALSE);

  if(!gnc_parser_configure_for_input_version(pstatus, version)) return(FALSE);

  return(TRUE);
}

static sixtp *
gnc_version_parser_new(void) 
{
  return(simple_chars_only_parser_new(gnc_version_end_handler));
}

/****************************************************************************/
/* <gnc> (lineage #f)
   
   Takes the results from various children and puts them in the
   global_data result structure.

   from parent: NA
   for children: NA
   result: NA
   -----------
   start: NA
   before-child: make sure we don't get two ledger-data's (not allowed ATM).
   after-child: if a ledger-data child, parse_data->root_account = *result.
   characters: allow_and_ignore_only_whitespace

   Similarly, only one query is allowed ... 
   end: NA

   cleanup-result: NA
   cleanup-chars: NA
   fail: NA
   result-fail: NA
   chars-fail: NA

 */

static gboolean
gnc_parser_before_child_handler(gpointer data_for_children,
                              GSList* data_from_children,
                              GSList* sibling_data,
                              gpointer parent_data,
                              gpointer global_data,
                              gpointer *result,
                              
                              const gchar *tag,
                              const gchar *child_tag) 
{  
  GNCParseStatus *pstatus = (GNCParseStatus *) global_data;
  
  g_return_val_if_fail(pstatus, FALSE);

  if(strcmp(child_tag, "ledger-data") == 0) {
    if(pstatus->root_account) {
      return(FALSE);
    }
  }

#if 0
  if(strcmp(child_tag, "query-server") == 0) {
    if(pstatus->query) return(FALSE);
  }
#endif
  return(TRUE);
}

static gboolean
gnc_parser_after_child_handler(gpointer data_for_children,
                               GSList* data_from_children,
                               GSList* sibling_data,
                               gpointer parent_data,
                               gpointer global_data,
                               gpointer *result,
                               const gchar *tag,
                               const gchar *child_tag,
                               sixtp_child_result *child_result)
{
  GNCParseStatus *pstatus = (GNCParseStatus *) global_data;
  g_return_val_if_fail(pstatus, FALSE);

  if(strcmp(child_tag, "ledger-data") == 0) 
  {
    g_return_val_if_fail(child_result, FALSE);
    g_return_val_if_fail(child_result->data, FALSE);
    pstatus->root_account = (Account *) child_result->data;
    child_result->should_cleanup = FALSE;
  }

#if 0
  if(strcmp(child_tag, "query-server") == 0) 
  {
    g_return_val_if_fail(child_result, FALSE);
    g_return_val_if_fail(child_result->data, FALSE);
    pstatus->query = (Query *) child_result->data;
    child_result->should_cleanup = FALSE;
  }
#endif
  return(TRUE);
}

static sixtp*
gnc_parser_new(void) 
{
    return sixtp_set_any(
        sixtp_new(), FALSE,
        SIXTP_CHARACTERS_HANDLER_ID, allow_and_ignore_only_whitespace,
        SIXTP_BEFORE_CHILD_HANDLER_ID, gnc_parser_before_child_handler,
        SIXTP_AFTER_CHILD_HANDLER_ID, gnc_parser_after_child_handler,
        SIXTP_NO_MORE_HANDLERS);
}

/* ================================================================== */

static sixtp *
gncxml_setup_for_read (GNCParseStatus *global_parse_status)
{

  sixtp *top_level_pr;
  sixtp *gnc_pr;
  sixtp *gnc_version_pr;

  /* top-level: This is just a dummy node.  It doesn't do anything.
     For now, the result is communicated through the global_data
     parser. */
  top_level_pr = sixtp_new();
  g_return_val_if_fail(top_level_pr, FALSE);
  sixtp_set_chars(top_level_pr, allow_and_ignore_only_whitespace);

  /* <gnc> */
  gnc_pr = gnc_parser_new();
  if(!gnc_pr) {
    sixtp_destroy(top_level_pr);
    return(NULL);
  }
  sixtp_add_sub_parser(top_level_pr, "gnc", gnc_pr);

  /* <version> */
  gnc_version_pr = gnc_version_parser_new();
  if(!gnc_version_pr) {
    sixtp_destroy(top_level_pr);
    return(NULL);
  }
  sixtp_add_sub_parser(gnc_pr, "version", gnc_version_pr);

  global_parse_status->seen_version = FALSE;
  global_parse_status->gnc_parser = gnc_pr;
  global_parse_status->root_account = NULL;
  global_parse_status->pricedb = NULL;
  //  global_parse_status->query = NULL;
  global_parse_status->error = GNC_PARSE_ERR_NONE;

  return top_level_pr;
}

/* ================================================================== */

gboolean
qof_session_load_from_xml_file(QofBook *book, const char *filename)
{
  gboolean parse_ok;
  gpointer parse_result = NULL;
  sixtp *top_level_pr;
  GNCParseStatus global_parse_status;
  Account *root;

  global_parse_status.book = book;
  g_return_val_if_fail(book, FALSE);
  g_return_val_if_fail(filename, FALSE);

  xaccDisableDataScrubbing();
  top_level_pr = gncxml_setup_for_read (&global_parse_status);
  g_return_val_if_fail(top_level_pr, FALSE);

  parse_ok = sixtp_parse_file(top_level_pr,
                              filename,
                              NULL,
                              &global_parse_status,
                              &parse_result);

  sixtp_destroy(top_level_pr);
  xaccEnableDataScrubbing();

  if(parse_ok) 
  {
    if(!global_parse_status.root_account) return FALSE;

    root = global_parse_status.root_account;
    gnc_book_set_root_account(book, root);

    /* Fix account and transaction commodities */
    xaccAccountTreeScrubCommodities (root);

    /* Fix split amount/value */
    xaccAccountTreeScrubSplits (root);

    return(TRUE);
  } else {
    return(FALSE);
  }
}

/* ================================================================== */

gboolean
gnc_is_xml_data_file(const gchar *filename) 
{
    return gnc_is_our_xml_file(filename, "gnc", NULL);
}

/* ================================================================== */
#include "qof.h"

/****************************************************************************/
/* <kvp-frame>
   
   A collection of node functions intended to parse a sub-node set
   that looks like this:

     <kvp-frame>
       <s>
         <k>notes</k>
         <string>foo</string>
       </s>
       <s>
         <k>temp</k>
         <gint64>97</gint64>
       </s>
     </kvp-frame>

   and return a kvp_frame*.  The start handler for the top allocates
   the kvp_frame* and passes it to the children.  The <s> blocks add
   slots according to their <k> (key) and value blocks.

   FIXME: right now this is totally inappropriate for cases where we
   want to read in a set of new values that should "merge" with the
   existing values.  This is only appropriate for wholesale
   replacement of the slots.

*/

/* kvp-frame [value] handlers

   Handle the possible values.  Each value handler is expected to
   parse it's subtree and return an appropriate kvp_value* in its
   result.  The <kvp-frame> <slot> handler will then cram it where it
   belongs. */


static void
kvp_value_result_cleanup(sixtp_child_result *cr)
{  
  kvp_value *v = (kvp_value *) cr->data;;
  if(v) kvp_value_delete(v);
}

static sixtp*
simple_kvp_value_parser_new(sixtp_end_handler end_handler) 
{
    return sixtp_set_any(sixtp_new(), FALSE,
                         SIXTP_CHARACTERS_HANDLER_ID,
                         generic_accumulate_chars,
                         SIXTP_END_HANDLER_ID, end_handler,
                         SIXTP_CLEANUP_RESULT_ID, kvp_value_result_cleanup,
                         SIXTP_CLEANUP_CHARS_ID, sixtp_child_free_data,
                         SIXTP_RESULT_FAIL_ID, kvp_value_result_cleanup,
                         SIXTP_CHARS_FAIL_ID, sixtp_child_free_data,
                         SIXTP_NO_MORE_HANDLERS);
}

/* <gint64> - gint64 kvp_value parser.

   input: NA
   returns: gint64 kvp_value

   start: NA
   chars: generic_accumulate_chars.
   end: convert chars to gint64 kvp_value* if possible and return.

   cleanup-result: kvp_value_delete.
   cleanup-chars: g_free (for chars)
   fail: NA
   result-fail: kvp_value_delete
   chars-fail: g_free (for chars)

 */

/* ------------------------------------------------------------ */
/* generic type copnversion for kvp types */
#define KVP_CVT_VALUE(TYPE)					\
{								\
  gchar *txt = NULL;						\
  TYPE val;							\
  kvp_value *kvpv;						\
  gboolean ok;							\
								\
  txt = concatenate_child_result_chars(data_from_children);	\
  g_return_val_if_fail(txt, FALSE);				\
  								\
  ok = (gboolean) string_to_##TYPE(txt, &val);			\
  g_free(txt);							\
  g_return_val_if_fail(ok, FALSE);				\
								\
  kvpv = kvp_value_new_##TYPE(val);				\
  g_return_val_if_fail(kvpv, FALSE);				\
    								\
  *result = kvpv;						\
  return(TRUE);							\
}
/* ------------------------------------------------------------ */

static gboolean
gint64_kvp_value_end_handler(gpointer data_for_children,
                             GSList* data_from_children,
                             GSList* sibling_data,
                             gpointer parent_data,
                             gpointer global_data,
                             gpointer *result,
                             const gchar *tag) 
{
    KVP_CVT_VALUE (gint64);
}


static sixtp*
gint64_kvp_value_parser_new(void) {
  return(simple_kvp_value_parser_new(gint64_kvp_value_end_handler));
}

static gboolean
double_kvp_value_end_handler(gpointer data_for_children,
                             GSList* data_from_children,
                             GSList* sibling_data,
                             gpointer parent_data,
                             gpointer global_data,
                             gpointer *result,
                             const gchar *tag) 
{
    KVP_CVT_VALUE (double);
}

static sixtp*
double_kvp_value_parser_new(void) {
  return(simple_kvp_value_parser_new(double_kvp_value_end_handler));
}

static gboolean
gnc_numeric_kvp_value_end_handler(gpointer data_for_children,
                                  GSList* data_from_children,
                                  GSList* sibling_data,
                                  gpointer parent_data,
                                  gpointer global_data,
                                  gpointer *result,
                                  const gchar *tag) 
{
    KVP_CVT_VALUE (gnc_numeric);
}

static sixtp*
gnc_numeric_kvp_value_parser_new(void) {
  return(simple_kvp_value_parser_new(gnc_numeric_kvp_value_end_handler));
}

static gboolean
string_kvp_value_end_handler(gpointer data_for_children,
                        GSList* data_from_children,
                        GSList* sibling_data,
                        gpointer parent_data,
                        gpointer global_data,
                        gpointer *result,
                        const gchar *tag) 
{
  gchar *txt = NULL;
  kvp_value *kvpv;
  
  txt = concatenate_child_result_chars(data_from_children);
  g_return_val_if_fail(txt, FALSE);
  
  kvpv = kvp_value_new_string(txt);
  g_free(txt);
  g_return_val_if_fail(kvpv, FALSE);
    
  *result = kvpv;
  return(TRUE);
}

static sixtp*
string_kvp_value_parser_new(void) {
  return(simple_kvp_value_parser_new(string_kvp_value_end_handler));
}

/* the guid handler is almost the same as above, but has 
 * inconsistent type handling */
static gboolean
guid_kvp_value_end_handler(gpointer data_for_children,
                           GSList* data_from_children,
                           GSList* sibling_data,
                           gpointer parent_data,
                           gpointer global_data,
                           gpointer *result,
                           const gchar *tag) 
{
  gchar *txt = NULL;
  GUID val;
  kvp_value *kvpv;
  gboolean ok;

  txt = concatenate_child_result_chars(data_from_children);
  g_return_val_if_fail(txt, FALSE);

  ok = string_to_guid(txt, &val);
  g_free(txt);

  g_return_val_if_fail(ok, FALSE);

  kvpv = kvp_value_new_guid(&val);
  g_return_val_if_fail(kvpv, FALSE);
   
  *result = kvpv;
  return(TRUE);
}

static sixtp*
guid_kvp_value_parser_new(void) {
  return(simple_kvp_value_parser_new(guid_kvp_value_end_handler));
}

/*********************************/
/* kvp-frame binary value handlers

   A binary chunk can have a variety of types of children, and these
   children may appear multiple times, but at the moment only <hex>
   children are supported.  The end handler has to take all the
   children's results, concatenate them into one big kvp_value, and
   return it.

   All of the children ATM are expected to return binary kvp_values.  */

/* <hex> (lineage <binary> <s> <kvp-frame>)
   input: NA
   returns: binary kvp_value

   start: NA
   chars: generic_accumulate_chars
   end: convert the chars from hex to binary data and return binary kvp_value.

   cleanup-result: kvp_value_delete
   cleanup-chars: g_free chars
   fail: NA
   result-fail: kvp_value_delete
   chars-fail: g_free chars
   
 */

static gboolean
hex_binary_kvp_value_end_handler(gpointer data_for_children,
                                 GSList  *data_from_children, GSList *sibling_data,
                                 gpointer parent_data, gpointer global_data,
                                 gpointer *result, const gchar *tag)
{
  gchar *txt = NULL;
  void *val;
  guint64 size;
  kvp_value *kvpv;
  gboolean ok;
  
  txt = concatenate_child_result_chars(data_from_children);
  g_return_val_if_fail(txt, FALSE);
  
  ok = hex_string_to_binary(txt, &val, &size);
  g_free(txt);

  g_return_val_if_fail(ok, FALSE);

  kvpv = kvp_value_new_binary_nc(val, size);
  g_return_val_if_fail(kvpv, FALSE);
    
  *result = kvpv;
  return(TRUE);
}

static sixtp*
hex_binary_kvp_value_parser_new(void) {
  return(simple_kvp_value_parser_new(hex_binary_kvp_value_end_handler));
}

/* <binary> (lineage <s> <kvp-frame>)
   input: NA
   returns: binary kvp_value*

   start: NA
   chars: allow_and_ignore_only_whitespace.
   end: concatenate all the binary data from the children -> kvp_value.

   cleanup-result: kvp_value_delete
   cleanup-chars: NA
   fail: NA
   result-fail: kvp_value_delete
   chars-fail: NA

 */

static gboolean
kvp_frame_binary_end_handler(gpointer data_for_children,
                             GSList  *data_from_children, GSList *sibling_data,
                             gpointer parent_data, gpointer global_data,
                             gpointer *result, const gchar *tag)
{
  void *data;
  guint64 total_size;
  guint64 pos;
  kvp_value *kvpv;
  GSList *lp;
  
  /* at this point, we know that if there are child results, they all
     have to be binary kvp_values. */
  
  /* first see how much data we've got. */
  total_size = 0;
  for(lp = data_from_children; lp; lp = lp->next) {
    sixtp_child_result *cr = (sixtp_child_result *) lp->data;
    kvp_value *kvp = (kvp_value *) cr->data;
    void *tmpdata;
    guint64 tmpsize;

    tmpdata = kvp_value_get_binary(kvp, &tmpsize);
    g_return_val_if_fail(tmpdata, FALSE);
    total_size += tmpsize;
  }

  /* allocate a chunk to hold it all and copy */
  data = g_new(gchar, total_size);
  g_return_val_if_fail(data, FALSE);

  pos = 0;
  for(lp = data_from_children; lp; lp = lp->next) {
    sixtp_child_result *cr = (sixtp_child_result *) lp->data;
    kvp_value *kvp = (kvp_value *) cr->data;
    void *new_data;
    guint64 new_size;

    new_data = kvp_value_get_binary(kvp, &new_size);
    g_return_val_if_fail(new_data, FALSE);
    memcpy((data + pos), new_data, new_size);
    pos += new_size;
  }

  kvpv = kvp_value_new_binary_nc(data, total_size);
  g_return_val_if_fail(kvpv, FALSE);

  *result = kvpv;
  return(TRUE);
}

static sixtp*
binary_kvp_value_parser_new(void) 
{
    return sixtp_add_some_sub_parsers(
        sixtp_set_any(sixtp_new(), FALSE,
                      SIXTP_CHARACTERS_HANDLER_ID,
                      allow_and_ignore_only_whitespace,
                      SIXTP_END_HANDLER_ID, kvp_frame_binary_end_handler,
                      SIXTP_CLEANUP_RESULT_ID, kvp_value_result_cleanup,
                      SIXTP_RESULT_FAIL_ID, kvp_value_result_cleanup,
                      SIXTP_NO_MORE_HANDLERS),
        TRUE,
        "hex", hex_binary_kvp_value_parser_new(),
        0);
}

/*********************************/
/* glist kvp-value handler
 */

/* <glist> (lineage <s> <kvp-frame>)
   input: NA
   returns: glist kvp_value

   start: NA
   chars: allow_and_ignore_only_whitespace
   end: convert the child list pointer to a glist kvp_value and return.

   cleanup-result: kvp_value_delete
   cleanup-chars: NA
   fail: NA
   result-fail: kvp_value_delete
   chars-fail: NA
   
 */


static gboolean
glist_kvp_value_end_handler(gpointer data_for_children,
                            GSList  *data_from_children, GSList *sibling_data,
                            gpointer parent_data, gpointer global_data,
                            gpointer *result, const gchar *tag)
{
  GSList *lp;
  GList *result_glist;
  kvp_value *kvp_result;

  result_glist = NULL;
  for(lp = data_from_children; lp; lp = lp->next) {
    sixtp_child_result *cr = (sixtp_child_result *) lp->data;
    kvp_value *kvp = (kvp_value *) cr->data;

    /* children are in reverse chron order, so this fixes it. */
    result_glist = g_list_prepend(result_glist, kvp);
    cr->should_cleanup = FALSE;
  }

  kvp_result = kvp_value_new_glist_nc(result_glist);
  if(!kvp_result) {
    kvp_glist_delete(result_glist);
  }
  *result = kvp_result;
  return(TRUE);
}

/* ---------------------------------------------- */
#define KVP_TOKEN(NAME,TOK)			\
  child_pr = NAME##_kvp_value_parser_new();	\
  g_return_val_if_fail(child_pr, FALSE);	\
  sixtp_add_sub_parser(p, TOK, child_pr);
/* ---------------------------------------------- */


static gboolean
add_all_kvp_value_parsers_as_sub_nodes(sixtp *p,
                                       sixtp *kvp_frame_parser,
                                       sixtp *glist_parser) {
  sixtp *child_pr;

  g_return_val_if_fail(p, FALSE);
  g_return_val_if_fail(kvp_frame_parser, FALSE);

  KVP_TOKEN(gint64, "gint64");
  KVP_TOKEN(double, "double");
  KVP_TOKEN(gnc_numeric, "numeric");
  KVP_TOKEN(string, "string");
  KVP_TOKEN(guid,   "guid");
  KVP_TOKEN(binary, "binary");

  sixtp_add_sub_parser(p, "glist", glist_parser);
  sixtp_add_sub_parser(p, "frame", kvp_frame_parser);

  return(TRUE);
}

static sixtp*
glist_kvp_value_parser_new(sixtp *kvp_frame_parser) {
    sixtp *top_level = sixtp_set_any(
        sixtp_new(), FALSE,
        SIXTP_CHARACTERS_HANDLER_ID, allow_and_ignore_only_whitespace,
        SIXTP_END_HANDLER_ID, glist_kvp_value_end_handler,
        SIXTP_CLEANUP_RESULT_ID, kvp_value_result_cleanup,
        SIXTP_RESULT_FAIL_ID, kvp_value_result_cleanup,
        SIXTP_NO_MORE_HANDLERS);
    if(!top_level)
    {
        return NULL;
    }
  
    if(!add_all_kvp_value_parsers_as_sub_nodes(top_level,
                                               kvp_frame_parser,
                                               top_level)) {
        sixtp_destroy(top_level);
        return(NULL);
    }

    return(top_level);
}

/*********************************/
/* kvp-frame slot handlers 
   
   handlers for the <s><k>some key</k><[value]>data</[value]> sub-structure.
*/

/* <k> (lineage <s> <kvp-frame>)

   kvp-frame slot key handler - just a generic-string-parser

 */

/* <s> (lineage <kvp-frame>)

   kvp-frame slot handler.

   input: kvp_frame*
   returns: NA

   start: NA
   characters: allow_and_ignore_only_whitespace
   end: check for two children - one must be a <k> - if OK, set slot.

   cleanup-result: NA
   cleanup-chars: NA
   fail: NA
   result-fail: NA
   chars-fail: NA

 */

static gboolean
kvp_frame_slot_end_handler(gpointer data_for_children,
                           GSList  *data_from_children, GSList *sibling_data,
                           gpointer parent_data, gpointer global_data,
                           gpointer *result, const gchar *tag)
{
  kvp_frame *f = (kvp_frame *) parent_data;
  GSList *lp;
  guint64 key_node_count;
  gchar *key = NULL;
  sixtp_child_result *value_cr = NULL;
  kvp_value *value = NULL;
  gboolean delete_value = FALSE;

  g_return_val_if_fail(f, FALSE);

  if(g_slist_length(data_from_children) != 2) return(FALSE);

  /* check to see that we got exactly one <key> node */
  lp = data_from_children;
  key_node_count = 0;
  for(lp = data_from_children; lp; lp = lp->next) {
    sixtp_child_result *cr = (sixtp_child_result *) lp->data;

    if(is_child_result_from_node_named(cr, "k")) {
      key = (char *) cr->data;
      key_node_count++;
    } else {
      if(is_child_result_from_node_named(cr, "frame")) {
        kvp_frame *frame = cr->data;
        value = kvp_value_new_frame (frame);
        delete_value = TRUE;
      } else {
        value = cr->data;
        delete_value = FALSE;
      }

      value_cr = cr;
    }
  }

  if(key_node_count != 1) return(FALSE);

  value_cr->should_cleanup = TRUE;
  kvp_frame_set_slot(f, key, value);
  if (delete_value)
    kvp_value_delete (value);
  return(TRUE);
}

static sixtp*
kvp_frame_slot_parser_new(sixtp *kvp_frame_parser) {
  sixtp *top_level;
  sixtp *child_pr;
  sixtp *glist_pr;

  g_return_val_if_fail(kvp_frame_parser, NULL);

  if(!(top_level = sixtp_set_any(
           sixtp_new(), FALSE,
           SIXTP_CHARACTERS_HANDLER_ID, allow_and_ignore_only_whitespace,
           SIXTP_END_HANDLER_ID, kvp_frame_slot_end_handler,
           SIXTP_NO_MORE_HANDLERS)))
  {
      return NULL;
  }

  child_pr = simple_chars_only_parser_new(NULL);
  if(!child_pr) { sixtp_destroy(top_level); return(NULL); }
  sixtp_add_sub_parser(top_level, "k", child_pr);

  glist_pr = glist_kvp_value_parser_new(kvp_frame_parser);
  if(!glist_pr) { sixtp_destroy(top_level); return(NULL); }

  if(!add_all_kvp_value_parsers_as_sub_nodes(top_level,
                                             kvp_frame_parser,
                                             glist_pr)) {
    sixtp_destroy(top_level);
    return(NULL);
  }

  return(top_level);
}


/* <kvp-frame> - can be used anywhere.

   input: NA
   returns: kvp_frame*

   start: Allocates kvp_frame* and places in data_for_children.
   characters: none (whitespace only).
   end: put kvp_frame* into result if everything's OK.

   cleanup-result: delete kvp_frame*
   cleanup-chars: NA
   fail: delete kvp_frame*
   result-fail: delete kvp_frame*
   chars-fail: NA

 */

static gboolean
kvp_frame_start_handler(GSList* sibling_data, gpointer parent_data,
                        gpointer global_data, gpointer *data_for_children,
                        gpointer *result, const gchar *tag, gchar **attrs)
{
  kvp_frame *f = kvp_frame_new();
  g_return_val_if_fail(f, FALSE);
  *data_for_children = f;
  return(TRUE);
}

static gboolean
kvp_frame_end_handler(gpointer data_for_children,
                      GSList  *data_from_children, GSList *sibling_data,
                      gpointer parent_data, gpointer global_data,
                      gpointer *result, const gchar *tag)
{
  kvp_frame *f = (kvp_frame *) data_for_children;
  g_return_val_if_fail(f, FALSE);
  *result = f;
  return(TRUE);
}

static void
kvp_frame_fail_handler(gpointer data_for_children,
                         GSList* data_from_children,
                         GSList* sibling_data,
                         gpointer parent_data,
                         gpointer global_data,
                         gpointer *result,
                         const gchar *tag)
{
  kvp_frame *f = (kvp_frame *) data_for_children;
  if(f) kvp_frame_delete(f);
}

static void
kvp_frame_result_cleanup(sixtp_child_result *cr)
{
  kvp_frame *f = (kvp_frame *) cr->data;;
  if(f) kvp_frame_delete(f);
}

static sixtp*
kvp_frame_parser_new(void) 
{
  sixtp *top_level;

  if(!(top_level = sixtp_set_any(
           sixtp_new(), FALSE,
           SIXTP_START_HANDLER_ID, kvp_frame_start_handler,
           SIXTP_CHARACTERS_HANDLER_ID, allow_and_ignore_only_whitespace,
           SIXTP_END_HANDLER_ID, kvp_frame_end_handler,
           SIXTP_CLEANUP_RESULT_ID, kvp_frame_result_cleanup,
           SIXTP_RESULT_FAIL_ID, kvp_frame_result_cleanup,
           SIXTP_FAIL_HANDLER_ID, kvp_frame_fail_handler,
           SIXTP_NO_MORE_HANDLERS)))
  {
      return NULL;
  }

  if(!(sixtp_add_some_sub_parsers(
           top_level, TRUE,
           "s", kvp_frame_slot_parser_new(top_level),
           0)))
  {
      return NULL;
  }

  return(top_level);
}

/****************************************************************************/
/****************************************************************************/
/****************************************************************************/
/* <ledger-data> (parent <gnc-data>)

   On failure or on normal cleanup, the root account will be killed,
   so if you want it, you better set should_cleanup to false

   input: NA
   to-children-via-*result: new root Account*
   returns: an Account*
   start: creates the root account and puts it into *result
   characters: NA
   end: finishes up the root account and leaves it in result.
   cleanup-result: deletes the root account (use should_cleanup to avoid).
   cleanup-chars: NA
   fail: deletes the root account in *result.
   result-fail: same as cleanup-result.
   chars-fail: NA

*/


static gboolean
ledger_data_start_handler(GSList* sibling_data, gpointer parent_data,
                          gpointer global_data, gpointer *data_for_children,
                          gpointer *result, const gchar *tag, gchar **attrs)
{
  GNCParseStatus *pstatus = (GNCParseStatus *) global_data;
  Account *ra;

  /* disable logging during load; otherwise its just a mess */
  xaccLogDisable();
  ra = xaccMallocAccount(pstatus->book);

  g_return_val_if_fail(ra, FALSE);

  *data_for_children = ra;
  return(ra != NULL);
}

static gboolean
ledger_data_after_child_handler(gpointer data_for_children,
                                GSList* data_from_children,
                                GSList* sibling_data,
                                gpointer parent_data,
                                gpointer global_data,
                                gpointer *result,
                                const gchar *tag,
                                const gchar *child_tag,
                                sixtp_child_result *child_result)
{
  if(!child_result) return(TRUE);

  /* if we see the pricedb, deal with it */
  if(child_result->type != SIXTP_CHILD_RESULT_NODE) return(TRUE);
  if(strcmp(child_result->tag, "pricedb") == 0) {
    GNCPriceDB *pdb = (GNCPriceDB *) child_result->data;
    GNCParseStatus *status = (GNCParseStatus *) global_data;

    g_return_val_if_fail(pdb, FALSE);
    g_return_val_if_fail(status, FALSE);

    if(status->pricedb) {
      PERR("hit pricedb twice in data file.");
      return FALSE;
    }
    status->pricedb = pdb;
    child_result->should_cleanup = FALSE;
  }
  return(TRUE);
}

static gboolean
ledger_data_end_handler(gpointer data_for_children,
                        GSList  *data_from_children, GSList *sibling_data,
                        gpointer parent_data, gpointer global_data,
                        gpointer *result, const gchar *tag)
{
  
  Account *ra = (Account *) data_for_children;
  GList *descendants;

  g_return_val_if_fail(ra, FALSE);

  /* commit all accounts, this completes the BeginEdit started when the
   * account_end_handler finished reading the account.
   */
  descendants = gnc_account_get_descendants(ra);
  g_list_foreach(descendants, (GFunc)xaccAccountCommitEdit, NULL);
  g_list_free(descendants);

  xaccLogEnable();

  *result = ra;
  return(TRUE);
}

static void
ledger_data_fail_handler(gpointer data_for_children,
                         GSList* data_from_children,
                         GSList* sibling_data,
                         gpointer parent_data,
                         gpointer global_data,
                         gpointer *result,
                         const gchar *tag)
{
  Account *account = (Account *) data_for_children;
  if (account)
  {
    xaccAccountBeginEdit(account);
    xaccAccountDestroy(account);
  }
}

static void
ledger_data_result_cleanup(sixtp_child_result *cr)
{
  Account *account = (Account *) cr->data;
  if (account)
  {
    xaccAccountBeginEdit(account);
    xaccAccountDestroy(account);
  }
}


static sixtp*
ledger_data_parser_new(void) 
{
  sixtp *top_level;
  
  /* <ledger-data> */
  if(!(top_level = sixtp_set_any(
           sixtp_new(), FALSE,
           SIXTP_START_HANDLER_ID, ledger_data_start_handler,
           SIXTP_CHARACTERS_HANDLER_ID, allow_and_ignore_only_whitespace,
           SIXTP_AFTER_CHILD_HANDLER_ID, ledger_data_after_child_handler,
           SIXTP_END_HANDLER_ID, ledger_data_end_handler,
           SIXTP_CLEANUP_RESULT_ID, ledger_data_result_cleanup,
           SIXTP_FAIL_HANDLER_ID, ledger_data_fail_handler,
           SIXTP_RESULT_FAIL_ID, ledger_data_result_cleanup,
           SIXTP_NO_MORE_HANDLERS)))
  {
      return NULL;
  }

  if(!sixtp_add_some_sub_parsers(
         top_level, TRUE,
         "commodity", commodity_restore_parser_new(),
         "pricedb", gnc_pricedb_parser_new(),
         "account", gnc_account_parser_new(),
         "transaction", gnc_transaction_parser_new(),
         0))
  {
      return NULL;
  }

  return(top_level);
}

/***********************************************************************/
/****************************************************************************/
/* <account> (parent <ledger-data>)
 
   This block does nothing but pass the ledger-data account group down
   to its children.  It generates no data of its own, so it doesn't
   need any cleanup.

   input: Account*

   to-children-via-*result: Account*

   returns: NA
   
   start: pass input to children.

   characters: NA

   end: NA

   cleanup-result: NA

   cleanup-chars: NA

   fail: NA

   result-fail: NA

   chars-fail: NA

 */

static gboolean
account_start_handler(GSList* sibling_data,
                      gpointer parent_data,
                      gpointer global_data,
                      gpointer *data_for_children,
                      gpointer *result,
                      const gchar *tag,
                      gchar **attrs)
{
  /* pass the parent data down to the children */
  *data_for_children = parent_data;
  return(TRUE);
}

/****************************************************************************/
/* <restore> (lineage <account> <ledger-data>)
   
   restores a given account.  We allocate the new account in the
   start block, the children modify it, and in the end block, we see
   if the resultant account is OK, and if so, we add it to the
   ledger-data's account group.
 
   input: Account*
   to-children-via-*result: new Account*
   returns: NA
   start: create new Account*, and leave in for children.
   characters: NA
   end: clear *result
   cleanup-result: NA
   cleanup-chars: NA
   fail: delete Account*
   result-fail: NA
   chars-fail: NA
 */

static gboolean
account_restore_start_handler(GSList* sibling_data,
                              gpointer parent_data,
                              gpointer global_data,
                              gpointer *data_for_children,
                              gpointer *result,
                              const gchar *tag,
                              gchar **attrs)
{
  GNCParseStatus *pstatus = (GNCParseStatus *) global_data;
  Account *acc = xaccMallocAccount(pstatus->book);
  
  g_return_val_if_fail(acc, FALSE);
  xaccAccountBeginEdit(acc);

  *data_for_children = acc;
  *result = acc;

  return(TRUE);
}

static gboolean
account_restore_end_handler(gpointer data_for_children,
                            GSList  *data_from_children, GSList *sibling_data,
                            gpointer parent_data, gpointer global_data,
                            gpointer *result, const gchar *tag)
{
  Account *parent = (Account *) parent_data;
  Account *acc = (Account *) *result;

  g_return_val_if_fail((parent && acc), FALSE);

  /* CHECKME: do we need to xaccAccountRecomputeBalance(acc) here? */
  xaccAccountCommitEdit(acc);

  /* If the account doesn't have a parent yet, just cram it into the
     top level */
  if (!gnc_account_get_parent(acc))
    gnc_account_append_child(parent, acc);

  *result = NULL;

  /* Now return the account to the "edit" state.  At the end of reading
   * all the transactions, we will Commit.  This replaces #splits
   *  rebalances with #accounts rebalances at the end.  A BIG win!
   */
  xaccAccountBeginEdit(acc);
  return(TRUE);
}

static gboolean
account_restore_after_child_handler(gpointer data_for_children,
                                    GSList* data_from_children,
                                    GSList* sibling_data,
                                    gpointer parent_data,
                                    gpointer global_data,
                                    gpointer *result,
                                    const gchar *tag,
                                    const gchar *child_tag,
                                    sixtp_child_result *child_result)
{
  Account *a = (Account *) data_for_children;
  /* GNCParseStatus *pstatus = (GNCParseStatus *) global_data; */

  g_return_val_if_fail(a, FALSE);

  if(!child_result) return(TRUE);
  if(child_result->type != SIXTP_CHILD_RESULT_NODE) return(TRUE);
  if(strcmp(child_result->tag, "slots") == 0) {
    kvp_frame *f = (kvp_frame *) child_result->data;
    g_return_val_if_fail(f, FALSE);
    if(a->inst.kvp_data) kvp_frame_delete(a->inst.kvp_data);
    a->inst.kvp_data = f;
    child_result->should_cleanup = FALSE;
  }
  else if(strcmp(child_result->tag, "currency") == 0) {
    gnc_commodity *com = (gnc_commodity *) child_result->data;
    g_return_val_if_fail(com, FALSE);
    if(DxaccAccountGetCurrency(a)) return FALSE;
    DxaccAccountSetCurrency(a, com);
    /* let the normal child_result handler clean up com */
  }
  else if(strcmp(child_result->tag, "security") == 0) {
    gnc_commodity *com = (gnc_commodity *) child_result->data;
    g_return_val_if_fail(com, FALSE);
    if(DxaccAccountGetSecurity(a)) return FALSE;
    DxaccAccountSetSecurity(a, com);
    /* let the normal child_result handler clean up com */
  }

  return(TRUE);
}

static void
account_restore_fail_handler(gpointer data_for_children,
                             GSList* data_from_children,
                             GSList* sibling_data,
                             gpointer parent_data,
                             gpointer global_data,
                             gpointer *result,
                             const gchar *tag)
{
  Account *acc = (Account *) *result;
  if(acc)
  {
    xaccAccountBeginEdit (acc);
    xaccAccountDestroy(acc);
  }
}

/****************************************************************************/
/* <name> (lineage <restore> <account>)
   
   restores a given account's name.
   input: Account*
   returns: NA

   start: NA
   characters: return string copy for accumulation in end handler.
   end: concatenate all chars and set as account name.

   cleanup-result: NA
   cleanup-chars: g_free the result string.
   fail: NA
   result-fail: NA
   chars-fail: g_free the result string.

 */
static gboolean
acc_restore_name_end_handler(gpointer data_for_children,
                             GSList  *data_from_children, GSList *sibling_data,
                             gpointer parent_data, gpointer global_data,
                             gpointer *result, const gchar *tag)
{
  Account *acc = (Account *) parent_data;
  gchar *name = NULL;

  g_return_val_if_fail(acc, FALSE);

  name = concatenate_child_result_chars(data_from_children);
  g_return_val_if_fail(name, FALSE);
  
  xaccAccountSetName(acc, name);
  g_free(name);
  return(TRUE);
}

/****************************************************************************/
/* <guid> (lineage <restore> <account>)
   
   restores a given account's guid.
   input: Account*
   returns: NA

   start: NA
   characters: return string copy for accumulation in end handler.
   end: concatenate all chars and set as account GUID if not duplicate.

   cleanup-result: NA
   cleanup-chars: g_free the result string.
   fail: NA
   result-fail: NA
   chars-fail: g_free the result string.

 */

static gboolean
acc_restore_guid_end_handler(gpointer data_for_children,
                             GSList  *data_from_children, GSList *sibling_data,
                             gpointer parent_data, gpointer global_data,
                             gpointer *result, const gchar *tag)
{
  GNCParseStatus *pstatus = (GNCParseStatus *) global_data;
  Account *acc = (Account *) parent_data;
  gchar *txt = NULL;
  GUID gid;
  gboolean ok;

  g_return_val_if_fail(acc, FALSE);

  txt = concatenate_child_result_chars(data_from_children);
  g_return_val_if_fail(txt, FALSE);
  
  ok = string_to_guid(txt, &gid);
  g_free(txt);

  g_return_val_if_fail(ok, FALSE);

  if(xaccAccountLookup(&gid, pstatus->book)) 
  {
    return(FALSE);
  }

  xaccAccountSetGUID(acc, &gid);
  return(TRUE);
}

/****************************************************************************/
/* <type> (lineage <restore> <account>)
   
   restores a given account's type.
   input: Account*
   returns: NA

   start: NA
   characters: return string copy for accumulation in end handler.
   end: concatenate all chars and set as account type.

   cleanup-result: NA
   cleanup-chars: g_free the result string.
   fail: NA
   result-fail: NA
   chars-fail: g_free the result string.

 */

static gboolean
acc_restore_type_end_handler(gpointer data_for_children,
                             GSList  *data_from_children, GSList *sibling_data,
                             gpointer parent_data, gpointer global_data,
                             gpointer *result, const gchar *tag)
{
  Account *acc = (Account *) parent_data;
  gchar *txt = NULL;
  int type;
  gboolean ok;

  g_return_val_if_fail(acc, FALSE);

  txt = concatenate_child_result_chars(data_from_children);
  g_return_val_if_fail(txt, FALSE);
  
  ok = xaccAccountStringToType(txt, &type);
  g_free(txt);

  g_return_val_if_fail(ok, FALSE);
  
  xaccAccountSetType(acc, type);
  return(TRUE);
}

/****************************************************************************/
/* <code> (lineage <restore> <account>)
   
   restores a given account's code.
   input: Account*
   returns: NA

   start: NA
   characters: return string copy for accumulation in end handler.
   end: concatenate all chars and set as account type.

   cleanup-result: NA
   cleanup-chars: g_free the result string.
   fail: NA
   result-fail: NA
   chars-fail: g_free the result string.

 */

static gboolean
acc_restore_code_end_handler(gpointer data_for_children,
                             GSList  *data_from_children, GSList *sibling_data,
                             gpointer parent_data, gpointer global_data,
                             gpointer *result, const gchar *tag)
{
  Account *acc = (Account *) parent_data;
  gchar *txt = NULL;
  
  g_return_val_if_fail(acc, FALSE);
  
  txt = concatenate_child_result_chars(data_from_children);
  g_return_val_if_fail(txt, FALSE);
  
  xaccAccountSetCode(acc, txt);
  g_free(txt);
  return(TRUE);
}

/****************************************************************************/
/* <description> (lineage <restore> <account>)
   
   restores a given account's description.
   input: Account*
   returns: NA

   start: NA
   characters: return string copy for accumulation in end handler.
   end: concatenate all chars and set as account description.

   cleanup-result: NA
   cleanup-chars: g_free the result string.
   fail: NA
   result-fail: NA
   chars-fail: g_free the result string.
   restores a given account's description.

 */

static gboolean
acc_restore_description_end_handler(gpointer data_for_children,
                                    GSList  *data_from_children, GSList *sibling_data,
                                    gpointer parent_data, gpointer global_data,
                                    gpointer *result, const gchar *tag)
{
  Account *acc = (Account *) parent_data;
  gchar *txt = NULL;
  
  g_return_val_if_fail(acc, FALSE);
  
  txt = concatenate_child_result_chars(data_from_children);
  g_return_val_if_fail(txt, FALSE);
  
  xaccAccountSetDescription(acc, txt);
  g_free(txt);
  return(TRUE);
}

/****************************************************************************/
/* <notes> (lineage <restore> <account>)
   
   restores a given account's notes.
   input: Account*
   returns: NA

   start: NA
   characters: return string copy for accumulation in end handler.
   end: concatenate all chars and set as account notes.

   cleanup-result: NA
   cleanup-chars: g_free the result string.
   fail: NA
   result-fail: NA
   chars-fail: g_free the result string.

 */

static gboolean
acc_restore_notes_end_handler(gpointer data_for_children,
                              GSList  *data_from_children, GSList *sibling_data,
                              gpointer parent_data, gpointer global_data,
                              gpointer *result, const gchar *tag)
{
  Account *acc = (Account *) parent_data;
  gchar *txt = NULL;
  
  g_return_val_if_fail(acc, FALSE);
  
  txt = concatenate_child_result_chars(data_from_children);
  g_return_val_if_fail(txt, FALSE);
  
  xaccAccountSetNotes(acc, txt);
  g_free(txt);
  return(TRUE);
}

/****************************************************************************/
/* <parent> (lineage <restore> <account>)
   
   restores a given account's parent.
   input: Account*
   returns: NA

   start: NA

   characters: allow and ignore only whitespace.

   end: check for single <guid> child and if found, use result to set
   account guid.

   cleanup-result: NA
   cleanup-chars: NA
   fail: NA
   result-fail: NA
   chars-fail: NA

 */

static gboolean
acc_restore_parent_end_handler(gpointer data_for_children,
                               GSList  *data_from_children, GSList *sibling_data,
                               gpointer parent_data, gpointer global_data,
                               gpointer *result, const gchar *tag)
{
  GNCParseStatus *pstatus = (GNCParseStatus *) global_data;
  Account *acc = (Account *) parent_data;
  Account *parent;
  sixtp_child_result *child_result;
  GUID gid;
  
  g_return_val_if_fail(acc, FALSE);

  if(g_slist_length(data_from_children) != 1)
    return(FALSE);

  child_result = (sixtp_child_result *) data_from_children->data;
  
  if(!is_child_result_from_node_named(child_result, "guid"))
    return(FALSE);

  /* otherwise this must be a good result - use it */
  gid = *((GUID *) child_result->data);

  parent = xaccAccountLookup(&gid, pstatus->book);
  
  g_return_val_if_fail(parent, FALSE);

  gnc_account_append_child(parent, acc);

  return(TRUE);
}

static sixtp *
parent_lookup_parser_new(void)
{
    return sixtp_set_any(sixtp_new(), TRUE,
                         SIXTP_CHARACTERS_HANDLER_ID,
                         allow_and_ignore_only_whitespace,
                         SIXTP_END_HANDLER_ID,
                         acc_restore_parent_end_handler,
                         SIXTP_NO_MORE_HANDLERS);
}

static sixtp *
gnc_account_parser_new(void)
{
  sixtp *restore_pr;
  sixtp *ret;
    
  /* <account> */
  if(!(ret = sixtp_set_any(
           sixtp_new(), FALSE,
           SIXTP_START_HANDLER_ID, account_start_handler,
           SIXTP_CHARACTERS_HANDLER_ID, allow_and_ignore_only_whitespace,
           SIXTP_NO_MORE_HANDLERS)))
  {
      return NULL;
  }
  
  /* <account> <restore> */
  if(!(restore_pr =
       sixtp_set_any(sixtp_new(), FALSE,
                     SIXTP_START_HANDLER_ID, account_restore_start_handler,
                     SIXTP_END_HANDLER_ID, account_restore_end_handler,
                     SIXTP_FAIL_HANDLER_ID, account_restore_fail_handler,
                     SIXTP_AFTER_CHILD_HANDLER_ID,
                     account_restore_after_child_handler,
                     SIXTP_NO_MORE_HANDLERS)))
  {
      sixtp_destroy(ret);
      return NULL;
  }
  
  /* <restore> (<name> | <guid> | <type> | <code> | <description> | <notes>)*/
  if(!sixtp_add_some_sub_parsers(
         restore_pr, TRUE,
         "name", restore_char_generator(acc_restore_name_end_handler),
         "guid", restore_char_generator(acc_restore_guid_end_handler),
         "type", restore_char_generator(acc_restore_type_end_handler),
         "code", restore_char_generator(acc_restore_code_end_handler),
         "description",
         restore_char_generator(acc_restore_description_end_handler),
         "notes", restore_char_generator(acc_restore_notes_end_handler),
         /* <account> <restore> <currency> */
         "currency", generic_gnc_commodity_lookup_parser_new(),
         /* <account> <restore> <security> */
         "security", generic_gnc_commodity_lookup_parser_new(),
         /* <account> <restore> <parent> */
         "parent", sixtp_add_some_sub_parsers(
             parent_lookup_parser_new(), TRUE,
             "guid", generic_guid_parser_new(),
             0),
         "slots", kvp_frame_parser_new(),
         0))
  {
      sixtp_destroy(ret);
      return NULL;
  }
  
  sixtp_add_sub_parser(ret, "restore", restore_pr);

  return ret;
}
/***********************************************************************/
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
  CommodityParseInfo *cpi =
    (CommodityParseInfo *) g_new0(CommodityParseInfo, 1);

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
  GNCParseStatus *pstatus = (GNCParseStatus *) global_data;
  gboolean ok = FALSE;
  gnc_commodity *comm = NULL;

  g_return_val_if_fail(cpi, FALSE);

  if(cpi->seen_fraction) {
    gnc_commodity *comm;

    if(!cpi->space) cpi->space = g_strdup("");
    if(!cpi->id) cpi->id = g_strdup("");
    if(!cpi->name) cpi->name = g_strdup("");
    if(!cpi->xcode) cpi->xcode = g_strdup("");

    comm = gnc_commodity_new(pstatus->book,
			     cpi->name,
                             cpi->space,
                             cpi->id,
                             cpi->xcode,
                             cpi->fraction);
    if(comm)
    {
      gnc_commodity_table *ctab;

      ctab = gnc_book_get_commodity_table (pstatus->book);

      if(ctab)
      {
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

  if(!ok) gnc_commodity_destroy(comm);

  return(ok);
}


static sixtp *
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
  GNCParseStatus *pstatus = (GNCParseStatus *) global_data;
  gboolean ok = FALSE;

  g_return_val_if_fail(cpi, FALSE);

  if(cpi->namespace && cpi->id) {
    gnc_commodity_table *table;
    gnc_commodity *com;

    table = gnc_book_get_commodity_table (pstatus->book);

    com = gnc_commodity_table_lookup(table, cpi->namespace, cpi->id);

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


static sixtp *
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

#if 0
/***********************************************************************/
/* <query-server> (parent <gnc-data>)

   On failure or on normal cleanup, the query will be killed,
   so if you want it, you better set should_cleanup to false

   input: NA
   to-children-via-*result: new Query*
   returns: a Query*
   start: creates the query and puts it into *result
   characters: NA
   end: finishes up the query and leaves it in result.
   cleanup-result: deletes the query (use should_cleanup to avoid).
   cleanup-chars: NA
   fail: deletes the query in *result.
   result-fail: same as cleanup-result.
   chars-fail: NA

*/


static gboolean
query_server_start_handler(GSList* sibling_data,
                           gpointer parent_data,
                           gpointer global_data,
                           gpointer *data_for_children,
                           gpointer *result,
                           const gchar *tag,
                           gchar **attrs)
{
  return(TRUE);
}

static gboolean
query_server_end_handler(gpointer data_for_children,
                         GSList  *data_from_children, GSList *sibling_data,
                         gpointer parent_data, gpointer global_data,
                         gpointer *result, const gchar *tag)
{
  Query *q;
  sixtp_child_result *cr;

  g_return_val_if_fail(data_from_children, FALSE);

  cr = (sixtp_child_result *) data_from_children->data;
  g_return_val_if_fail(cr, FALSE);

  q = (Query *) (cr->data);
  g_return_val_if_fail(q, FALSE);

  *result = q;
  return(TRUE);
}


/* ================================================================= */
/* <query> (parent <query-server>)
 
   This block does nothing.
   It generates no data of its own, so it doesn't need any cleanup.

   input: NA
   to-children-via-*result: NA
   returns: NA
   start: NA.
   characters: NA
   end: NA
   cleanup-result: NA
   cleanup-chars: NA
   fail: NA
   result-fail: NA
   chars-fail: NA

 */

static gboolean
query_start_handler(GSList* sibling_data, gpointer parent_data,
                    gpointer global_data, gpointer *data_for_children,
                    gpointer *result, const gchar *tag, gchar **attrs)
{
  return(TRUE);
}

static gboolean
query_end_handler(gpointer data_for_children,
                  GSList  *data_from_children, GSList *sibling_data,
                  gpointer parent_data, gpointer global_data,
                  gpointer *result, const gchar *tag)
{
  Query *q;
  sixtp_child_result *cr;

  g_return_val_if_fail(data_from_children, FALSE);

  cr = (sixtp_child_result *) data_from_children->data;
  g_return_val_if_fail(cr, FALSE);

  q = (Query *) (cr->data);
  g_return_val_if_fail(q, FALSE);

  *result = q;
  return(TRUE);
}

/* ================================================================= */
/* <restore> (lineage <query> <query-server>)
   
   restores a given query.  We allocate the new query in the
   start block, the children modify it, and in the end block, we see
   if the resultant query is OK, and if so, we're done.
 
   input: Query*
   to-children-via-*result: new Query*
   returns: NA
   start: create new Query*, and leave in for children.
   characters: NA
   end: clear *result
   cleanup-result: NA
   cleanup-chars: NA
   fail: delete Query*
   result-fail: NA
   chars-fail: NA

 */

static gboolean
query_restore_start_handler(GSList* sibling_data, gpointer parent_data,
                            gpointer global_data, gpointer *data_for_children,
                            gpointer *result, const gchar *tag, gchar **attrs)
{
  Query *q;
  q = xaccMallocQuery();
  g_return_val_if_fail(q, FALSE);
  *data_for_children = q;
  *result = q;
  return(q != NULL);
}

static gboolean
query_restore_end_handler(gpointer data_for_children,
                          GSList  *data_from_children, GSList *sibling_data,
                          gpointer parent_data, gpointer global_data,
                          gpointer *result, const gchar *tag)
{
  sixtp_child_result *cr;
  Query *qand, *qret;
  Query *q = (Query *) data_for_children;
  g_return_val_if_fail(q, FALSE);

  g_return_val_if_fail(data_from_children, FALSE);
  cr = (sixtp_child_result *) data_from_children->data;
  g_return_val_if_fail(cr, FALSE);

  qand = (Query *) (cr->data);
  g_return_val_if_fail(qand, FALSE);

  /* append the and terms by or'ing them in ... */
  qret = xaccQueryMerge (q, qand, QUERY_OR);
  if (!qret) {
    xaccFreeQuery(qand);
    *result = q;
    g_return_val_if_fail(qret, FALSE);
  }

  xaccFreeQuery(q);
  xaccFreeQuery(qand);

  *result = qret;
  return(TRUE);
}

static gboolean
query_restore_after_child_handler(gpointer data_for_children,
                           GSList* data_from_children,
                           GSList* sibling_data,
                           gpointer parent_data,
                           gpointer global_data,
                           gpointer *result,
                           const gchar *tag,
                           const gchar *child_tag,
                           sixtp_child_result *child_result)
{  
  return(TRUE);
}

static void
query_restore_fail_handler(gpointer data_for_children,
                         GSList* data_from_children,
                         GSList* sibling_data,
                         gpointer parent_data,
                         gpointer global_data,
                         gpointer *result,
                         const gchar *tag)
{  
  Query *q = (Query *) data_for_children;
  if (q) xaccFreeQuery(q);
}

/* ================================================================= */
/* <and-terms> (lineage <restore> <query> <query-server>)
   
   restores a given query.  We allocate the new query in the
   start block, the children modify it, and in the end block, we see
   if the resultant query is OK, and if so, we're done.
 
   input: Query*
   to-children-via-*result: new Query*
   returns: NA
   start: create new Query*, and leave in for children.
   characters: NA
   end: clear *result
   cleanup-result: NA
   cleanup-chars: NA
   fail: delete Query*
   result-fail: NA
   chars-fail: NA

 */

static gboolean
query_and_start_handler(GSList* sibling_data, gpointer parent_data,
                        gpointer global_data, gpointer *data_for_children,
                        gpointer *result, const gchar *tag, gchar **attrs)
{
  Query *q;

  /* note this malloc freed in the node higher up (query_restore_end_handler) */
  q = xaccMallocQuery();
  g_return_val_if_fail(q, FALSE);
  *data_for_children = q;
  *result = q;
  return(q != NULL);
}

static gboolean
query_and_end_handler(gpointer data_for_children,
                      GSList  *data_from_children, GSList *sibling_data,
                      gpointer parent_data, gpointer global_data,
                      gpointer *result, const gchar *tag)
{
  Query *q = (Query *) data_for_children;
  g_return_val_if_fail(q, FALSE);
  *result = q;
  return(TRUE);
}

static void
query_and_fail_handler(gpointer data_for_children,
                         GSList* data_from_children,
                         GSList* sibling_data,
                         gpointer parent_data,
                         gpointer global_data,
                         gpointer *result,
                         const gchar *tag)
{  
  Query *q = (Query *) data_for_children;
  if (q) xaccFreeQuery(q);
}

/* ================================================================= */

#define CVT_INT(to) {							\
  gint32 val;								\
  gboolean ok;								\
  gchar *txt = NULL;							\
									\
  txt = concatenate_child_result_chars(data_from_children);		\
  g_return_val_if_fail(txt, FALSE);					\
									\
  ok = (gboolean) string_to_gint32(txt, &val);				\
  g_free(txt);								\
  g_return_val_if_fail(ok, FALSE);					\
  (to) = val;								\
}

#define CVT_DATE(to) {							\
  TimespecParseInfo *info = (TimespecParseInfo *) data_for_children;	\
  									\
  g_return_val_if_fail(info, FALSE);					\
  if(!timespec_parse_ok(info)) {					\
    g_free(info);							\
    return(FALSE);							\
  }									\
									\
  to = info->ts;							\
  g_free(info);								\
}

/* ================================================================= */

static gboolean
qrestore_genericpred_end_handler(gpointer data_for_children,
                                 GSList  *data_from_children, GSList *sibling_data,
                                 gpointer parent_data, gpointer global_data,
                                 gpointer *result, const gchar *tag)
{
  Query *q = (Query *) parent_data;
  PredicateData *dp = (PredicateData *) data_for_children;

  g_return_val_if_fail(q, FALSE);
  g_return_val_if_fail(dp, FALSE);

  xaccQueryAddPredicate (q, dp, QUERY_AND);

  return(TRUE);
}

/* ================================================================= */
/* <datepred> (lineage <and-terms> <restore> <query> <query-server>)
   Restores a given date predicate.  
 
   from parent: Query*
   for children: NA
   result: NA
   -----------
   start: malloc a date predicate
   chars: allow and ignore only whitespace.
   end: AddDateMatch to Query
   cleanup-result: NA
   cleanup-chars: NA
   fail: ??
   result-fail: NA
   chars-fail: NA
 */

static gboolean
qrestore_datepred_start_handler(GSList* sibling_data, gpointer parent_data,
                                gpointer global_data,
                                gpointer *data_for_children,
                                gpointer *result, const gchar *tag,
                                gchar **attrs)
{
  DatePredicateData *dp = g_new0 (DatePredicateData, 1);
  g_return_val_if_fail(dp, FALSE);
  dp->type = PD_DATE;
  dp->term_type = PR_DATE;
  *data_for_children = dp;
  return(TRUE);
}

static void
qrestore_datepred_fail_handler(gpointer data_for_children,
                         GSList* data_from_children,
                         GSList* sibling_data,
                         gpointer parent_data,
                         gpointer global_data,
                         gpointer *result,
                         const gchar *tag)
{
  // g_free (data_for_children);
}

/* ================================================================= */
/* <end-date> (lineage <date-pred> <and-terms> <restore> <query>)
   restores a given query's end-date.
   Just uses a generic_timespec parser, but with our own end handler.
   end: set end-date.
 */

static gboolean
datepred_use_start_end_handler(gpointer data_for_children,
                               GSList  *data_from_children, GSList *sibling_data,
                               gpointer parent_data, gpointer global_data,
                               gpointer *result, const gchar *tag)
{
  DatePredicateData *dp = (DatePredicateData *) parent_data;
  CVT_INT(dp->use_start);
  return(TRUE);
}

static gboolean
datepred_use_end_end_handler(gpointer data_for_children,
                             GSList  *data_from_children, GSList *sibling_data,
                             gpointer parent_data, gpointer global_data,
                             gpointer *result, const gchar *tag)
{
  DatePredicateData *dp = (DatePredicateData *) parent_data;
  CVT_INT(dp->use_end);
  return(TRUE);
}

static gboolean
datepred_start_date_end_handler(gpointer data_for_children,
                                GSList  *data_from_children, GSList *sibling_data,
                                gpointer parent_data, gpointer global_data,
                                gpointer *result, const gchar *tag)
{
  DatePredicateData *dp = (DatePredicateData *) parent_data;
  CVT_DATE (dp->start);
  return(TRUE);
}

static gboolean
datepred_end_date_end_handler(gpointer data_for_children,
                              GSList  *data_from_children, GSList *sibling_data,
                              gpointer parent_data, gpointer global_data,
                              gpointer *result, const gchar *tag)
{
  DatePredicateData *dp = (DatePredicateData *) parent_data;
  CVT_DATE (dp->end);
  return(TRUE);
}

static gboolean
generic_pred_sense_end_handler(gpointer data_for_children,
                               GSList  *data_from_children, GSList *sibling_data,
                               gpointer parent_data, gpointer global_data,
                               gpointer *result, const gchar *tag)
{
  PredicateData *dp = (PredicateData *) parent_data;
  CVT_INT(dp->base.sense);
  return(TRUE);
}

static sixtp*
pred_parser_new(sixtp_end_handler ender)
{
    return sixtp_set_any(simple_chars_only_parser_new(NULL), FALSE,
                         SIXTP_END_HANDLER_ID, ender,
                         SIXTP_NO_MORE_HANDLERS);
}

/* ================================================================= */

static sixtp*
qrestore_datepred_parser_new(void)
{
    return sixtp_add_some_sub_parsers(
        sixtp_new(), TRUE,
        "sense", pred_parser_new(generic_pred_sense_end_handler),
        "use-start", pred_parser_new(datepred_use_start_end_handler),
        "use-end", pred_parser_new(datepred_use_end_end_handler),
        "start-date",
        generic_timespec_parser_new(datepred_start_date_end_handler),
        "end-date", 
        generic_timespec_parser_new(datepred_end_date_end_handler),
        0);
}

static sixtp*
query_server_parser_new (void) 
{
  sixtp *top_level;
  sixtp *query_pr;
  sixtp *restore_pr;
  sixtp *and_pr;
  sixtp *date_pred_pr;
  
  /* <query_server> */
  if(!(top_level =
       sixtp_set_any(sixtp_new(), FALSE,
                     SIXTP_START_HANDLER_ID, query_server_start_handler,
                     SIXTP_CHARACTERS_HANDLER_ID,
                     allow_and_ignore_only_whitespace,
                     SIXTP_END_HANDLER_ID, query_server_end_handler,
                     SIXTP_NO_MORE_HANDLERS)))
  {
      return NULL;
  }

  /* <query_server> <query> */
  if(!(query_pr =
       sixtp_set_any(sixtp_new(), FALSE,
                     SIXTP_START_HANDLER_ID, query_start_handler,
                     SIXTP_CHARACTERS_HANDLER_ID,
                     allow_and_ignore_only_whitespace,
                     SIXTP_END_HANDLER_ID, query_end_handler,
                     SIXTP_NO_MORE_HANDLERS)))
  {
    sixtp_destroy(top_level);
    return (NULL);
  }
  sixtp_add_sub_parser(top_level, "query", query_pr);
  
  /* <query> <restore> */
  if(!(restore_pr = sixtp_set_any(
           sixtp_new(), FALSE,
           SIXTP_START_HANDLER_ID, query_restore_start_handler,
           SIXTP_END_HANDLER_ID, query_restore_end_handler,
           SIXTP_FAIL_HANDLER_ID, query_restore_fail_handler,
           SIXTP_AFTER_CHILD_HANDLER_ID, query_restore_after_child_handler,
           SIXTP_NO_MORE_HANDLERS)))
  {
      sixtp_destroy(top_level);
      return(NULL);
  }
  sixtp_add_sub_parser(query_pr, "restore", restore_pr);
  
  /* <query> <restore> <and-terms> */
  if(!(and_pr =
       sixtp_set_any(sixtp_new(), FALSE,
                     SIXTP_START_HANDLER_ID, query_and_start_handler,
                     SIXTP_CHARACTERS_HANDLER_ID,
                     allow_and_ignore_only_whitespace,
                     SIXTP_END_HANDLER_ID, query_and_end_handler,
                     SIXTP_FAIL_HANDLER_ID, query_and_fail_handler,
                     SIXTP_NO_MORE_HANDLERS)))
  {
    sixtp_destroy(top_level);
    return (NULL);
  }
  sixtp_add_sub_parser(restore_pr, "and-terms", and_pr);

  if(!(date_pred_pr =
       sixtp_set_any(qrestore_datepred_parser_new(), FALSE,
                     SIXTP_START_HANDLER_ID, qrestore_datepred_start_handler,
                     SIXTP_CHARACTERS_HANDLER_ID,
                     allow_and_ignore_only_whitespace,
                     SIXTP_END_HANDLER_ID, qrestore_genericpred_end_handler,
                     SIXTP_FAIL_HANDLER_ID, qrestore_datepred_fail_handler,
                     SIXTP_NO_MORE_HANDLERS)))
  {
      sixtp_destroy(top_level);
      return NULL;
  }
  sixtp_add_sub_parser(and_pr, "date-pred", date_pred_pr);

  return(top_level);
}
#endif /* 0 */

/***********************************************************************/
/****************************************************************************/
/* <transaction> (parent <ledger-data>)
 
   This block does nothing but pass the ledger-data account group down
   to its children.  It generates no data of its own, so it doesn't
   need any cleanup.

   input: Account*

   to-children-via-*result: Account*

   returns: NA
   
   start: pass input to children.

   characters: ignore whitespace only

   end: NA

   cleanup-result: NA

   cleanup-chars: NA

   fail: NA

   result-fail: NA

   chars-fail: NA

 */

static gboolean
transaction_start_handler(GSList* sibling_data, gpointer parent_data,
                          gpointer global_data, gpointer *data_for_children,
                          gpointer *result, const gchar *tag, gchar **attrs)
{
  /* pass the parent data down to the children */
  *data_for_children = parent_data;
  return(TRUE);
}

/****************************************************************************/
/* <restore> (lineage <transaction> <ledger-data>)
   
   restores a given transaction.  We allocate the new transaction in
   the start block, the children modify it, and in the end block, we
   see if the resultant account is OK, and if so, we add it to the
   ledger-data's account group.
 
   from parent: Account*

   for children: new Transaction*

   result: NA
   
   -----------

   start: create new Transaction*, and store in data_for_children.

   chars: allow and ignore only whitespace.

   end: commit transaction to group if appropriate.

   cleanup-result: NA

   cleanup-chars: NA

   fail: delete Transaction* in data_for_children

   result-fail: NA

   chars-fail: NA

 */

static gboolean
txn_restore_start_handler(GSList* sibling_data, gpointer parent_data,
                          gpointer global_data, gpointer *data_for_children,
                          gpointer *result, const gchar *tag, gchar **attrs)
{
  GNCParseStatus *pstatus = (GNCParseStatus *) global_data;
  Transaction *trans = xaccMallocTransaction(pstatus->book);

  g_return_val_if_fail(trans, FALSE);

  xaccTransBeginEdit(trans);
  *data_for_children = trans;

  return(TRUE);
}

static gboolean
txn_restore_end_handler(gpointer data_for_children,
                        GSList  *data_from_children, GSList *sibling_data,
                        gpointer parent_data, gpointer global_data,
                        gpointer *result, const gchar *tag)
{
  Account *parent = (Account *) parent_data;
  Transaction *trans = (Transaction *) data_for_children;

  g_return_val_if_fail(trans, FALSE);
  if (!parent) {
    xaccTransDestroy(trans);
    xaccTransCommitEdit(trans);
    return(FALSE);
  }

  if(!xaccTransGetGUID(trans)) {
    /* must at least have a GUID for a restore */
    xaccTransDestroy(trans);
    xaccTransCommitEdit(trans);
    return(FALSE);
  }
    
  /* FIXME: what if the trans has no splits? */
  xaccTransCommitEdit(trans);

  return(TRUE);
}

static gboolean
txn_restore_after_child_handler(gpointer data_for_children,
                           GSList* data_from_children,
                           GSList* sibling_data,
                           gpointer parent_data,
                           gpointer global_data,
                           gpointer *result,
                           const gchar *tag,
                           const gchar *child_tag,
                           sixtp_child_result *child_result)
{
  Transaction *trans = (Transaction *) data_for_children;
  g_return_val_if_fail(trans, FALSE);
  if(!child_result) return(TRUE);
  if(child_result->type != SIXTP_CHILD_RESULT_NODE) return(TRUE);
  if(strcmp(child_result->tag, "slots") == 0) {
    kvp_frame *f = (kvp_frame *) child_result->data;
    g_return_val_if_fail(f, FALSE);
    qof_instance_set_slots(QOF_INSTANCE(trans),f);
    child_result->should_cleanup = FALSE;
  }
  return(TRUE);
}

static void
txn_restore_fail_handler(gpointer data_for_children,
                         GSList* data_from_children,
                         GSList* sibling_data,
                         gpointer parent_data,
                         gpointer global_data,
                         gpointer *result,
                         const gchar *tag)
{
  Transaction *trans = (Transaction *) data_for_children;
  if(trans) {
    xaccTransDestroy(trans);
    xaccTransCommitEdit(trans);
  }
}

/****************************************************************************/
/* <guid> (lineage <restore> <transaction>)
   
   restores a given account's guid.

   from parent: Transaction*
   for children: NA
   result: NA
   -----------
   start: NA
   characters: return string copy for accumulation in end handler.
   end: concatenate all chars and set as transaction GUID if not duplicate.

   cleanup-result: NA
   cleanup-chars: g_free the result string.
   fail: NA
   result-fail: NA
   chars-fail: g_free the result string.

 */

static gboolean
txn_restore_guid_end_handler(gpointer data_for_children,
                             GSList  *data_from_children, GSList *sibling_data,
                             gpointer parent_data, gpointer global_data,
                             gpointer *result, const gchar *tag)
{
  GNCParseStatus *pstatus = (GNCParseStatus *) global_data;
  Transaction *t = (Transaction *) parent_data;
  gchar *txt = NULL;
  GUID gid;
  gboolean ok;

  g_return_val_if_fail(t, FALSE);

  txt = concatenate_child_result_chars(data_from_children);
  g_return_val_if_fail(txt, FALSE);
  
  ok = string_to_guid(txt, &gid);
  g_free(txt);

  g_return_val_if_fail(ok, FALSE);

  if(xaccTransLookup(&gid, pstatus->book)) 
  {
    return(FALSE);
  }

  xaccTransSetGUID(t, &gid);
  return(TRUE);
}

/****************************************************************************/
/* <num> (lineage <restore> <transaction>)
   
   restores a given transaction's num.

   from parent: Transaction*
   for children: NA
   result: NA
   -----------
   start: NA
   characters: return string copy for accumulation in end handler.
   end: concatenate all chars and set as transaction num.

   cleanup-result: NA
   cleanup-chars: g_free the result string.
   fail: NA
   result-fail: NA
   chars-fail: g_free the result string.

 */

static gboolean
txn_restore_num_end_handler(gpointer data_for_children,
                            GSList  *data_from_children, GSList *sibling_data,
                            gpointer parent_data, gpointer global_data,
                            gpointer *result, const gchar *tag)
{
  Transaction *t = (Transaction *) parent_data;
  gchar *txt = NULL;
  
  g_return_val_if_fail(t, FALSE);
  
  txt = concatenate_child_result_chars(data_from_children);
  g_return_val_if_fail(txt, FALSE);
  
  xaccTransSetNum(t, txt);
  g_free(txt);
  return(TRUE);
}

/****************************************************************************/
/* <description> (lineage <restore> <transaction>)
   
   restores a given transaction's description.

   from parent: Transaction*
   for children: NA
   result: NA
   -----------
   start: NA
   characters: return string copy for accumulation in end handler.
   end: concatenate all chars and set as transaction description.

   cleanup-result: NA
   cleanup-chars: g_free the result string.
   fail: NA
   result-fail: NA
   chars-fail: g_free the result string.

 */

static gboolean
txn_restore_description_end_handler(gpointer data_for_children,
                                    GSList  *data_from_children, GSList *sibling_data,
                                    gpointer parent_data, gpointer global_data,
                                    gpointer *result, const gchar *tag)
{
  Transaction *t = (Transaction *) parent_data;
  gchar *txt = NULL;
  
  g_return_val_if_fail(t, FALSE);
  
  txt = concatenate_child_result_chars(data_from_children);
  g_return_val_if_fail(txt, FALSE);
  
  xaccTransSetDescription(t, txt);
  g_free(txt);
  return(TRUE);
}

/****************************************************************************/
/* <date-posted> (lineage <restore> <transaction>)
   
   restores a given transaction's posted date.

   Just uses a generic_timespec parser, but with our own end handler.

   end: set date posted.

 */

static gboolean
txn_rest_date_posted_end_handler(gpointer data_for_children,
                                 GSList  *data_from_children, GSList *sibling_data,
                                 gpointer parent_data, gpointer global_data,
                                 gpointer *result, const gchar *tag)
{
  Transaction *t = (Transaction *) parent_data;
  TimespecParseInfo *info = (TimespecParseInfo *) data_for_children;
  
  g_return_val_if_fail(info, FALSE);
  if(!t || !timespec_parse_ok(info)) {
    g_free(info);
    return(FALSE);
  }

  xaccTransSetDatePostedTS(t, &(info->ts));
  g_free(info);
  return(TRUE);
}

/****************************************************************************/
/* <date-entered> (lineage <restore> <transaction>)
   
   restores a given transaction's entered date.

   Just uses a generic_timespec parser, but with our own end handler.

   end: set date entered.

 */

static gboolean
txn_rest_date_entered_end_handler(gpointer data_for_children,
                                  GSList  *data_from_children, GSList *sibling_data,
                                  gpointer parent_data, gpointer global_data,
                                  gpointer *result, const gchar *tag)
{
  Transaction *t = (Transaction *) parent_data;
  TimespecParseInfo *info = (TimespecParseInfo *) data_for_children;
  
  g_return_val_if_fail(info, FALSE);
  if(!t || !timespec_parse_ok(info)) {
    g_free(info);
    return(FALSE);
  }

  xaccTransSetDateEnteredTS(t, &(info->ts));
  g_free(info);
  return(TRUE);
}



/****************************************************************************/

/* <split> (lineage <restore> <transaction> <ledger-data>)
   
   Restores a given split.  We allocate the new split in the start
   block, the children modify it, and in the end block, we see if the
   resultant split is OK, and if so, we add it to the input Transaction*
   account group.
 
   from parent: Transaction*
   for children: new Split*
   result: NA
   -----------
   start: create new Split*, and store in data_for_children.
   chars: allow and ignore only whitespace.
   end: commit split to transaction if appropriate.
   cleanup-result: NA
   cleanup-chars: NA
   fail: delete Transaction* in data_for_children
   result-fail: NA
   chars-fail: NA

 */

static gboolean
txn_restore_split_start_handler(GSList* sibling_data, gpointer parent_data,
                                gpointer global_data,
                                gpointer *data_for_children, gpointer *result,
                                const gchar *tag, gchar **attrs)
{
  GNCParseStatus *pstatus = (GNCParseStatus *) global_data;
  Split *s = xaccMallocSplit(pstatus->book);
  g_return_val_if_fail(s, FALSE);
  *data_for_children = s;
  return(TRUE);
}

static gboolean
txn_restore_split_end_handler(gpointer data_for_children,
                              GSList  *data_from_children, GSList *sibling_data,
                              gpointer parent_data, gpointer global_data,
                              gpointer *result, const gchar *tag)
{
  Transaction *t = (Transaction *) parent_data;
  Split *s = (Split *) data_for_children;

  g_return_val_if_fail(s, FALSE);
  if(!t) {
    xaccSplitDestroy(s);
    return(FALSE);
  }

  if(!xaccSplitGetGUID(s)) {
    /* must at least have a GUID for a restore */
    xaccSplitDestroy(s);
    return(FALSE);
  }
    
  xaccTransAppendSplit(t, s);
  return(TRUE);
}

static gboolean
txn_restore_split_after_child_handler(gpointer data_for_children,
                           GSList* data_from_children,
                           GSList* sibling_data,
                           gpointer parent_data,
                           gpointer global_data,
                           gpointer *result,
                           const gchar *tag,
                           const gchar *child_tag,
                           sixtp_child_result *child_result)
{
  Split *s = (Split *) data_for_children;
  g_return_val_if_fail(s, FALSE);
  if(!child_result) return(TRUE);
  if(child_result->type != SIXTP_CHILD_RESULT_NODE) return(TRUE);

  if(strcmp(child_result->tag, "slots") == 0) {
    kvp_frame *f = (kvp_frame *) child_result->data;
    g_return_val_if_fail(f, FALSE);
    if(s->inst.kvp_data) kvp_frame_delete(s->inst.kvp_data);
    s->inst.kvp_data = f;
    child_result->should_cleanup = FALSE;
  }
  else if(strcmp(child_result->tag, "quantity") == 0) {
    gnc_numeric *n = (gnc_numeric *) child_result->data;
    g_return_val_if_fail(n, FALSE);
    xaccSplitSetAmount(s, *n);
    /* let the normal child_result handler clean up n */
  }
  else if(strcmp(child_result->tag, "value") == 0) {
    gnc_numeric *n = (gnc_numeric *) child_result->data;
    g_return_val_if_fail(n, FALSE);
    xaccSplitSetValue(s, *n);
    /* let the normal child_result handler clean up n */
  }

  return(TRUE);
}

static void
txn_restore_split_fail_handler(gpointer data_for_children,
                         GSList* data_from_children,
                         GSList* sibling_data,
                         gpointer parent_data,
                         gpointer global_data,
                         gpointer *result,
                         const gchar *tag)
{
  Split *s = (Split *) data_for_children;
  if(s) xaccSplitDestroy(s);
}

/****************************************************************************/
/* <guid> (lineage <split> <restore> <transaction>)
   
   restores a given split's guid.

   from parent: Split*
   for children: NA
   result: NA
   -----------
   start: NA
   characters: return string copy for accumulation in end handler.
   end: concatenate all chars and set as split GUID if not duplicate.

   cleanup-result: NA
   cleanup-chars: g_free the result string.
   fail: NA
   result-fail: NA
   chars-fail: g_free the result string.

 */

static gboolean
txn_restore_split_guid_end_handler(gpointer data_for_children,
                                   GSList  *data_from_children, GSList *sibling_data,
                                   gpointer parent_data, gpointer global_data,
                                   gpointer *result, const gchar *tag)
{
  GNCParseStatus *pstatus = (GNCParseStatus *) global_data;
  Split *s = (Split *) parent_data;
  gchar *txt = NULL;
  GUID gid;
  gboolean ok;

  g_return_val_if_fail(s, FALSE);

  txt = concatenate_child_result_chars(data_from_children);
  g_return_val_if_fail(txt, FALSE);
  
  ok = string_to_guid(txt, &gid);
  g_free(txt);

  g_return_val_if_fail(ok, FALSE);

  if(xaccSplitLookup(&gid, pstatus->book)) 
  {
    return(FALSE);
  }

  xaccSplitSetGUID(s, &gid);
  return(TRUE);
}

/****************************************************************************/
/* <memo> (lineage <split> <restore> <transaction>)
   
   restores a given split's memo.

   from parent: Split*
   for children: NA
   result: NA
   -----------
   start: NA
   characters: return string copy for accumulation in end handler.
   end: concatenate all chars and set as split description.

   cleanup-result: NA
   cleanup-chars: g_free the result string.
   fail: NA
   result-fail: NA
   chars-fail: g_free the result string.

 */

static gboolean
txn_restore_split_memo_end_handler(gpointer data_for_children,
                                   GSList  *data_from_children, GSList *sibling_data,
                                   gpointer parent_data, gpointer global_data,
                                   gpointer *result, const gchar *tag)
{
  Split *s = (Split *) parent_data;
  gchar *txt = NULL;
  
  g_return_val_if_fail(s, FALSE);
  
  txt = concatenate_child_result_chars(data_from_children);
  g_return_val_if_fail(txt, FALSE);
  
  xaccSplitSetMemo(s, txt);
  g_free(txt);
  return(TRUE);
}

/****************************************************************************/
/* <action> (lineage <split> <restore> <transaction>)
   
   restores a given split's action.

   from parent: Split*
   for children: NA
   result: NA
   -----------
   start: NA
   characters: return string copy for accumulation in end handler.
   end: concatenate all chars and set as split action.

   cleanup-result: NA
   cleanup-chars: g_free the result string.
   fail: NA
   result-fail: NA
   chars-fail: g_free the result string.

 */

static gboolean
txn_restore_split_action_end_handler(gpointer data_for_children,
                                     GSList  *data_from_children, GSList *sibling_data,
                                     gpointer parent_data, gpointer global_data,
                                     gpointer *result, const gchar *tag)
{
  Split *s = (Split *) parent_data;
  gchar *txt = NULL;
  
  g_return_val_if_fail(s, FALSE);
  
  txt = concatenate_child_result_chars(data_from_children);
  g_return_val_if_fail(txt, FALSE);
  
  xaccSplitSetAction(s, txt);
  g_free(txt);
  return(TRUE);
}

/****************************************************************************/
/* <reconcile-state> (lineage <split> <restore> <transaction>)
   
   restores a given split's reconcile-state.

   from parent: Split*
   for children: NA
   result: NA
   -----------
   start: NA
   characters: return string copy for accumulation in end handler.
   end: concatenate all chars and set as split reconcile-state.

   cleanup-result: NA
   cleanup-chars: g_free the result string.
   fail: NA
   result-fail: NA
   chars-fail: g_free the result string.

 */

static gboolean
txn_restore_split_reconcile_state_end_handler(gpointer data_for_children,
                                              GSList  *data_from_children, GSList *sibling_data,
                                              gpointer parent_data, gpointer global_data,
                                              gpointer *result, const gchar *tag)
{
  Split *s = (Split *) parent_data;
  gchar *txt = NULL;
  
  g_return_val_if_fail(s, FALSE);
  
  txt = concatenate_child_result_chars(data_from_children);
  g_return_val_if_fail(txt, FALSE);
  
  if(strlen(txt) != 1) {
    g_free(txt);
    return(FALSE);
  }

  xaccSplitSetReconcile(s, txt[0]);
  g_free(txt);
  return(TRUE);
}

/****************************************************************************/
/* <reconcile-date> (lineage <split> <restore> <transaction>)
   
   restores a given split's reconcile-date.

   Just uses a generic_timespec parser, but with our own end handler.

   end: set reconcile-date.

 */

static gboolean
txn_restore_split_reconcile_date_end_handler(gpointer data_for_children,
                                             GSList  *data_from_children, GSList *sibling_data,
                                             gpointer parent_data, gpointer global_data,
                                             gpointer *result, const gchar *tag)
{
  Split *s = (Split *) parent_data;
  TimespecParseInfo *info = (TimespecParseInfo *) data_for_children;
  
  g_return_val_if_fail(info, FALSE);
  if(!s || !timespec_parse_ok(info)) {
    g_free(info);
    return(FALSE);
  }

  xaccSplitSetDateReconciledTS(s, &(info->ts));
  g_free(info);
  return(TRUE);
}

/****************************************************************************/
/* <account> (lineage <split> <restore> <transaction>)
   
   restores a given split's account.

   from parent: Split*
   for children: NA
   result: NA
   -----------
   start: NA
   characters: return string copy for accumulation in end handler.
   end: concatenate all chars and set as split account if GUID OK.

   cleanup-result: NA
   cleanup-chars: g_free the result string.
   fail: NA
   result-fail: NA
   chars-fail: g_free the result string.

 */

static gboolean
txn_restore_split_account_end_handler(gpointer data_for_children,
                                      GSList  *data_from_children, GSList *sibling_data,
                                      gpointer parent_data, gpointer global_data,
                                      gpointer *result, const gchar *tag)
{
  GNCParseStatus *pstatus = (GNCParseStatus *) global_data;
  Split *s = (Split *) parent_data;
  Account *acct;
  gchar *txt = NULL;
  GUID gid;
  gboolean ok;
  
  g_return_val_if_fail(s, FALSE);
  
  txt = concatenate_child_result_chars(data_from_children);
  g_return_val_if_fail(txt, FALSE);
  
  ok = string_to_guid(txt, &gid);
  g_free(txt);
  
  g_return_val_if_fail(ok, FALSE);
  
  acct = xaccAccountLookup(&gid, pstatus->book);
  g_return_val_if_fail(acct, FALSE);

  xaccAccountInsertSplit(acct, s);
  return(TRUE);
}


/****************************************************************************/


/****************************************************************************/

static sixtp *
gnc_txn_restore_split_parser_new(void) 
{
  sixtp *top_level;
  
  if(!(top_level =
       sixtp_set_any(sixtp_new(), FALSE,
                     SIXTP_START_HANDLER_ID, txn_restore_split_start_handler,
                     SIXTP_CHARACTERS_HANDLER_ID,
                     allow_and_ignore_only_whitespace,
                     SIXTP_END_HANDLER_ID, txn_restore_split_end_handler,
                     SIXTP_FAIL_HANDLER_ID, txn_restore_split_fail_handler,
                     SIXTP_AFTER_CHILD_HANDLER_ID,
                     txn_restore_split_after_child_handler,
                     SIXTP_NO_MORE_HANDLERS)))
  {
      return NULL;
  }

  if(!sixtp_add_some_sub_parsers(
         top_level, TRUE,
         "guid", restore_char_generator(txn_restore_split_guid_end_handler),
         "memo", restore_char_generator(txn_restore_split_memo_end_handler),
         "action",
         restore_char_generator(txn_restore_split_action_end_handler),
         "account",
         restore_char_generator(txn_restore_split_account_end_handler),
         "reconcile-state",
         restore_char_generator(txn_restore_split_reconcile_state_end_handler),
         "reconcile-date", 
         generic_timespec_parser_new(
             txn_restore_split_reconcile_date_end_handler),
         "quantity", generic_gnc_numeric_parser_new(),
         "value", generic_gnc_numeric_parser_new(),
         "slots", kvp_frame_parser_new(),
         0))
  {
      return NULL;
  }

  return(top_level);
}

/***************************************************************************/

static sixtp *
gnc_transaction_parser_new(void) 
{
  sixtp *top_level;
  sixtp *restore_pr;

  if(!(top_level =
       sixtp_set_any(sixtp_new(), FALSE,
                     SIXTP_START_HANDLER_ID, transaction_start_handler,
                     SIXTP_CHARACTERS_HANDLER_ID,
                     allow_and_ignore_only_whitespace,
                     SIXTP_AFTER_CHILD_HANDLER_ID,
                     txn_restore_after_child_handler,
                     SIXTP_NO_MORE_HANDLERS)))
  {
      return NULL;
  }

  /* <restore> */
  if(!(restore_pr =
       sixtp_set_any(sixtp_new(), FALSE,
                     SIXTP_START_HANDLER_ID, txn_restore_start_handler,
                     SIXTP_END_HANDLER_ID, txn_restore_end_handler,
                     SIXTP_FAIL_HANDLER_ID, txn_restore_fail_handler,
                     SIXTP_AFTER_CHILD_HANDLER_ID,
                     txn_restore_after_child_handler,
                     SIXTP_NO_MORE_HANDLERS)))
  {
      sixtp_destroy(top_level);
      return(NULL);
  }
  sixtp_add_sub_parser(top_level, "restore", restore_pr);

  if(!(sixtp_add_some_sub_parsers(
           restore_pr, TRUE,
           "guid", restore_char_generator(txn_restore_guid_end_handler),
           "num", restore_char_generator(txn_restore_num_end_handler),
           "description",
           restore_char_generator(txn_restore_description_end_handler),
           "date-posted",
           generic_timespec_parser_new(txn_rest_date_posted_end_handler),
           "date-entered",
           generic_timespec_parser_new(txn_rest_date_entered_end_handler),
           "slots", kvp_frame_parser_new(),
           "split", gnc_txn_restore_split_parser_new(),
           0)))
  {
      sixtp_destroy(top_level);
      return NULL;
  }

  return(top_level);
}

/****************************************************************************/
/****************************************************************************/

/* Read and Write the pricedb as XML -- something like this:

  <pricedb>
    price-1
    price-2
    ...
  </pricedb>

  where each price should look roughly like this:

  <price>
    <price:id>
      00000000111111112222222233333333
    </price:id>
    <price:commodity>
      <cmdty:space>NASDAQ</cmdty:space>
      <cmdty:id>RHAT</cmdty:id>
    </price:commodity>
    <price:currency>
      <cmdty:space>ISO?</cmdty:space>
      <cmdty:id>USD</cmdty:id>
    </price:currency>
    <price:time><ts:date>Mon ...</ts:date><ts:ns>12</ts:ns></price:time>
    <price:source>Finance::Quote</price:source>
    <price:type>bid</price:type>
    <price:value>11011/100</price:value>
  </price>

*/

/***********************************************************************/
/* READING */
/***********************************************************************/

/****************************************************************************/
/* <price>

  restores a price.  Does so via a walk of the XML tree in memory.
  Returns a GNCPrice * in result.

  Right now, a price is legitimate even if all of it's fields are not
  set.  We may need to change that later, but at the moment.  

*/

static gboolean
price_parse_xml_sub_node(GNCPrice *p, xmlNodePtr sub_node, QofBook *book)
{
  if(!p || !sub_node) return FALSE;

  gnc_price_begin_edit (p);

  if(safe_strcmp("price:id", (char*)sub_node->name) == 0) {
    GUID *c = dom_tree_to_guid(sub_node);
    if(!c) return FALSE; 
    gnc_price_set_guid(p, c);
    g_free(c);
  } else if(safe_strcmp("price:commodity", (char*)sub_node->name) == 0) {
    gnc_commodity *c = dom_tree_to_commodity_ref(sub_node, book);
    if(!c) return FALSE;
    gnc_price_set_commodity(p, c);
  } else if(safe_strcmp("price:currency", (char*)sub_node->name) == 0) {
    gnc_commodity *c = dom_tree_to_commodity_ref(sub_node, book);
    if(!c) return FALSE;
    gnc_price_set_currency(p, c);
  } else if(safe_strcmp("price:time", (char*)sub_node->name) == 0) {
    Timespec t = dom_tree_to_timespec(sub_node);
    if (!dom_tree_valid_timespec(&t, sub_node->name)) return FALSE;
    gnc_price_set_time(p, t);
  } else if(safe_strcmp("price:source", (char*)sub_node->name) == 0) {
    char *text = dom_tree_to_text(sub_node);
    if(!text) return FALSE;
    gnc_price_set_source(p, text);
    g_free(text);
  } else if(safe_strcmp("price:type", (char*)sub_node->name) == 0) {
    char *text = dom_tree_to_text(sub_node);
    if(!text) return FALSE;
    gnc_price_set_typestr(p, text);
    g_free(text);
  } else if(safe_strcmp("price:value", (char*)sub_node->name) == 0) {
    gnc_numeric *value = dom_tree_to_gnc_numeric(sub_node);
    if(!value) return FALSE;
    gnc_price_set_value(p, *value);
    g_free(value);
  }
  gnc_price_commit_edit (p);
  return TRUE;
}

static gboolean
price_parse_xml_end_handler(gpointer data_for_children,
                            GSList* data_from_children,
                            GSList* sibling_data,
                            gpointer parent_data,
                            gpointer global_data,
                            gpointer *result,
                            const gchar *tag)
{
  gboolean ok = TRUE;
  xmlNodePtr price_xml = (xmlNodePtr) data_for_children;
  xmlNodePtr child;
  GNCPrice *p = NULL;
  GNCParseStatus *pstatus = (GNCParseStatus *) global_data;

  /* we haven't been handed the *top* level node yet... */
  if(parent_data) return TRUE;

  *result = NULL;

  if(!price_xml) return FALSE;
  if(price_xml->next) { ok = FALSE; goto cleanup_and_exit; }
  if(price_xml->prev) { ok = FALSE; goto cleanup_and_exit; }
  if(!price_xml->xmlChildrenNode) { ok = FALSE; goto cleanup_and_exit; }
  
  p = gnc_price_create(pstatus->book);
  if(!p) { ok = FALSE; goto cleanup_and_exit; }
  
  for(child = price_xml->xmlChildrenNode; child; child = child->next) 
  {
    switch(child->type) {
      case XML_COMMENT_NODE:
      case XML_TEXT_NODE:
        break;
      case XML_ELEMENT_NODE:
        if(!price_parse_xml_sub_node(p, child, pstatus->book)) 
        {
          ok = FALSE;
          goto cleanup_and_exit;
        }
        break;
      default:
        PERR("Unknown node type (%d) while parsing gnc-price xml.", child->type);
        child = NULL;
        ok = FALSE;
        goto cleanup_and_exit;
        break;
    }
  }
  
 cleanup_and_exit:
  if(ok) {
    *result = p;
  } else {
    *result = NULL;
    gnc_price_unref(p);
  }
  xmlFreeNode(price_xml);
  return ok;
}

static void
cleanup_gnc_price(sixtp_child_result *result)
{
  if(result->data) gnc_price_unref((GNCPrice *) result->data);
}

static sixtp *
gnc_price_parser_new (void)
{
  return sixtp_dom_parser_new(price_parse_xml_end_handler,
                              cleanup_gnc_price,
                              cleanup_gnc_price);
}


/****************************************************************************/
/* <pricedb> (lineage <ledger-data>)
   
   restores a pricedb.  We allocate the new db in the start block, the
   children add to it, and it gets returned in result.  Note that the
   cleanup handler will destroy the pricedb, so the parent needs to
   stop that if desired.
 
   result: GNCPriceDB*

   start: create new GNCPriceDB*, and leave in *data_for_children.
   cleanup-result: destroy GNCPriceDB*
   result-fail: destroy GNCPriceDB*

*/

static gboolean
pricedb_start_handler(GSList* sibling_data,
                      gpointer parent_data,
                      gpointer global_data,
                      gpointer *data_for_children,
                      gpointer *result,
                      const gchar *tag,
                      gchar **attrs)
{
  GNCParseStatus *pstatus = (GNCParseStatus *) global_data;
  GNCPriceDB *db = gnc_book_get_pricedb(pstatus->book);
  g_return_val_if_fail(db, FALSE);
  *result = db;
  return(TRUE);
}

static gboolean
pricedb_after_child_handler(gpointer data_for_children,
                            GSList* data_from_children,
                            GSList* sibling_data,
                            gpointer parent_data,
                            gpointer global_data,
                            gpointer *result,
                            const gchar *tag,
                            const gchar *child_tag,
                            sixtp_child_result *child_result)
{
  GNCPriceDB *db = (GNCPriceDB *) *result;

  g_return_val_if_fail(db, FALSE);

  /* right now children have to produce results :> */
  if(!child_result) return(FALSE);
  if(child_result->type != SIXTP_CHILD_RESULT_NODE) return(FALSE);

  if(strcmp(child_result->tag, "price") == 0) {
    GNCPrice *p = (GNCPrice *) child_result->data;

    g_return_val_if_fail(p, FALSE);
    gnc_pricedb_add_price(db, p);
    return TRUE;
  } else {
    return FALSE;
  }
  return FALSE;
}

static void
pricedb_cleanup_result_handler(sixtp_child_result *result)
{
  if(result->data) {
    GNCPriceDB *db = (GNCPriceDB *) result->data;
    if(db) gnc_pricedb_destroy(db);
    result->data = NULL;
  }
}

static sixtp*
gnc_pricedb_parser_new(void) 
{
  sixtp *top_level;
  sixtp *price_parser;

  top_level = 
    sixtp_set_any(sixtp_new(), TRUE,
                  SIXTP_START_HANDLER_ID, pricedb_start_handler,
                  SIXTP_AFTER_CHILD_HANDLER_ID, pricedb_after_child_handler,
                  SIXTP_CHARACTERS_HANDLER_ID,
                  allow_and_ignore_only_whitespace,
                  SIXTP_RESULT_FAIL_ID, pricedb_cleanup_result_handler,
                  SIXTP_CLEANUP_RESULT_ID, pricedb_cleanup_result_handler,
                  SIXTP_NO_MORE_HANDLERS);

  if(!top_level) return NULL;

  price_parser = gnc_price_parser_new();

  if(!price_parser) {
    sixtp_destroy(top_level);
    return NULL;
  }

  sixtp_add_sub_parser(top_level, "price", price_parser);

  return top_level;
}

/* ======================= END OF FILE ============================== */
