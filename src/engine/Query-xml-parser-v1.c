/********************************************************************\
 * Query-xml-parser-v1.c                                            *
 * Copyright (C) 2000 Gnumatic, Inc.                                *
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
\********************************************************************/

#include "config.h"

#include <glib.h>

#include <string.h>

#include "sixtp.h"
#include "sixtp-utils.h"
#include "sixtp-parsers.h"
#include "sixtp-writers.h"
#include "sixtp-xml-write-utils.h"
#include "gnc-engine-util.h"

#include "Query.h"

static short module = MOD_IO;

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

sixtp*
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
