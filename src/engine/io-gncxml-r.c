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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
 *******************************************************************/

#include "config.h"

#include <stdlib.h>
#include <string.h>

#include <glib.h>

#include "gnc-xml-helper.h"
#include "Account.h"
#include "Query.h"
#include "gnc-pricedb.h"
#include "gnc-engine-util.h"
#include "gnc-book-p.h"

#include "io-gncxml.h"
#include "io-gncxml-p.h"

#include "sixtp.h"
#include "sixtp-stack.h"
#include "sixtp-parsers.h"
#include "sixtp-utils.h"

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

static short module = MOD_IO;

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

  /* add <query-server> */
  {
    sixtp *query_server_pr = query_server_parser_new();
    g_return_val_if_fail(query_server_pr, FALSE);
    sixtp_add_sub_parser(status->gnc_parser, "query-server", query_server_pr);
  }

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
   after-child: if a ledger-data child, parse_data->account_group = *result.
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
    if(pstatus->account_group) {
      return(FALSE);
    }
  }

  if(strcmp(child_tag, "query-server") == 0) {
    if(pstatus->query) return(FALSE);
  }
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
    pstatus->account_group = (AccountGroup *) child_result->data;
    child_result->should_cleanup = FALSE;
  }

  if(strcmp(child_tag, "query-server") == 0) 
  {
    g_return_val_if_fail(child_result, FALSE);
    g_return_val_if_fail(child_result->data, FALSE);
    pstatus->query = (Query *) child_result->data;
    child_result->should_cleanup = FALSE;
  }
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
  global_parse_status->account_group = NULL;
  global_parse_status->pricedb = NULL;
  global_parse_status->query = NULL;
  global_parse_status->error = GNC_PARSE_ERR_NONE;

  return top_level_pr;
}

/* ================================================================== */

gboolean
gnc_book_load_from_xml_file(GNCBook *book)
{
  gboolean parse_ok;
  gpointer parse_result = NULL;
  sixtp *top_level_pr;
  GNCParseStatus global_parse_status;
  const gchar *filename;

  g_return_val_if_fail(book, FALSE);

  filename = gnc_book_get_file_path(book);
  g_return_val_if_fail(filename, FALSE);

  top_level_pr = gncxml_setup_for_read (&global_parse_status);
  g_return_val_if_fail(top_level_pr, FALSE);

  parse_ok = sixtp_parse_file(top_level_pr,
                              filename,
                              NULL,
                              &global_parse_status,
                              &parse_result);

  sixtp_destroy(top_level_pr);

  if(parse_ok) {
    if(!global_parse_status.account_group)
      return FALSE;

    {
      AccountGroup *g = gnc_book_get_group(book);

      if(g) xaccFreeAccountGroup(g);
      gnc_book_set_group(book, global_parse_status.account_group);
    }

    if(global_parse_status.pricedb)
    {
      GNCPriceDB *db = gnc_book_get_pricedb(book);

      if(db) gnc_pricedb_destroy(db);

      gnc_book_set_pricedb(book, global_parse_status.pricedb);
    }
    else
    {
      GNCPriceDB *db = gnc_book_get_pricedb(book);

      if(db) gnc_pricedb_destroy(db);

      gnc_book_set_pricedb(book, gnc_pricedb_create());
    }
    return(TRUE);
  } else {
    return(FALSE);
  }
}

/* ================================================================== */

gboolean
gnc_is_xml_data_file(const gchar *filename) 
{
    return gnc_is_our_xml_file(filename, "gnc");
}

/* ================================================================== */

#if 0

static GNCParseStatus*
read_from_buf(char *bufp, int bufsz)
{
  gboolean parse_ok;
  gpointer parse_result = NULL;
  sixtp *top_level_pr;
  GNCParseStatus *global_parse_status;

  g_return_val_if_fail(bufp, NULL);

  global_parse_status = g_new0(GNCParseStatus, 1);
  g_return_val_if_fail(global_parse_status, NULL);
  
  top_level_pr = gncxml_setup_for_read (global_parse_status);
  g_return_val_if_fail(top_level_pr, NULL);

  parse_ok = sixtp_parse_buffer(top_level_pr,
                                bufp,
                                bufsz,
                                NULL,
                                global_parse_status,
                                &parse_result);
  
  sixtp_destroy(top_level_pr);

  if (parse_ok)
      return global_parse_status;
  else
      return NULL;
}

AccountGroup *
gncxml_read_from_buf (char * bufp, int bufsz)
{
    AccountGroup *ret;
    GNCParseStatus *info;

    info = read_from_buf(bufp, bufsz);
    g_return_val_if_fail(info, NULL);

    ret = info->account_group;

    g_free(info);

    return ret;
}

/* ================================================================== */

Query *
gncxml_read_query (char * bufp, int bufsz)
{
    Query *ret;
    GNCParseStatus *info;

    info = read_from_buf(bufp, bufsz);
    g_return_val_if_fail(info, NULL);

    ret = info->query;

    g_free(info);

    return ret;
}

#endif

/* ======================= END OF FILE ============================== */
