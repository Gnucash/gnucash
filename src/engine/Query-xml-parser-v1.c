#include <glib.h>

#include <string.h>

#include "sixtp.h"
#include "sixtp-utils.h"
#include "sixtp-parsers.h"

#include "Query.h"

/****************************************************************************/
/* ================================================================= */
/* ================================================================= */
/* ================================================================= */
/* ================================================================= */
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
                          const gchar *tag)
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
query_start_handler(GSList* sibling_data,
                          gpointer parent_data,
                          gpointer global_data,
                          gpointer *data_for_children,
                          gpointer *result,
                          const gchar *tag)
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
query_restore_start_handler(GSList* sibling_data,
                          gpointer parent_data,
                          gpointer global_data,
                          gpointer *data_for_children,
                          gpointer *result,
                          const gchar *tag)
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
query_and_start_handler(GSList* sibling_data,
                          gpointer parent_data,
                          gpointer global_data,
                          gpointer *data_for_children,
                          gpointer *result,
                          const gchar *tag)
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
qrestore_datepred_start_handler(GSList* sibling_data,
                          gpointer parent_data,
                          gpointer global_data,
                          gpointer *data_for_children,
                          gpointer *result,
                          const gchar *tag)
{
  DatePredicateData *dp = g_new (DatePredicateData, 1);
  g_return_val_if_fail(dp, FALSE);
  bzero (dp, sizeof (DatePredicateData));
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


/* --------------------------------------------------- */
#define PRED_PARSE(PRED,NAME,TOK)			\
{							\
  sixtp *tmp_pr = simple_chars_only_parser_new(NULL);	\
  if(!tmp_pr) {						\
    sixtp_destroy(top_level);				\
    return(NULL);					\
  }							\
  sixtp_set_end(tmp_pr, PRED##_##NAME##_end_handler);	\
  sixtp_add_sub_parser(top_level, TOK, tmp_pr);		\
}
/* --------------------------------------------------- */
/* ================================================================= */

static sixtp*
qrestore_datepred_parser_new(void)
{
  sixtp *top_level = sixtp_new();
  sixtp *restore_pr = top_level;
  g_return_val_if_fail(top_level, NULL);

  PRED_PARSE(generic_pred, sense,  "sense");
  PRED_PARSE(datepred, use_start,  "use-start");
  PRED_PARSE(datepred, use_end,    "use-end");
  sixtp_add_sub_parser(
      restore_pr, "start-date", 
      generic_timespec_parser_new(datepred_start_date_end_handler));
  sixtp_add_sub_parser(
      restore_pr, "end-date", 
      generic_timespec_parser_new(datepred_end_date_end_handler));

  return(top_level);
}

/* ================================================================= */
/* Generic predicate restorion macro */
#define RESTORE_PRED(NAME,TOK,REST)					\
  {									\
    sixtp *tmp_pr = REST##_##NAME##_parser_new();			\
    if(!tmp_pr) {							\
      sixtp_destroy(top_level);						\
      return(NULL);							\
    }									\
    sixtp_set_start(tmp_pr, REST##_##NAME##_start_handler);		\
    sixtp_set_chars(top_level, allow_and_ignore_only_whitespace);	\
    sixtp_set_end(tmp_pr, qrestore_genericpred_end_handler);		\
    /* sixtp_set_after_child(tmp_pr, REST##_##NAME##_after_child_handler); */	\
    sixtp_set_fail(tmp_pr, REST##_##NAME##_fail_handler);		\
    sixtp_add_sub_parser(and_pr, TOK, tmp_pr);				\
  }

/* ================================================================= */

sixtp*
query_server_parser_new (void) 
{
  sixtp *top_level;
  sixtp *query_pr;
  sixtp *restore_pr;
  sixtp *and_pr;
  
  /* <query_server> */
  top_level = sixtp_new();
  g_return_val_if_fail(top_level, NULL);
  sixtp_set_start(top_level, query_server_start_handler);
  sixtp_set_chars(top_level, allow_and_ignore_only_whitespace);
  sixtp_set_end(top_level, query_server_end_handler);

  /* <query_server> <query> */
  query_pr = sixtp_new();
  if (!query_pr) {
    sixtp_destroy(top_level);
    return (NULL);
  }
  sixtp_set_start(query_pr, query_start_handler);
  sixtp_set_chars(query_pr, allow_and_ignore_only_whitespace);
  sixtp_set_end(query_pr, query_end_handler);
  sixtp_add_sub_parser(top_level, "query", query_pr);
  
  /* <query> <restore> */
  restore_pr = setup_restorer(query_pr,
                              query_restore_start_handler,
                              query_restore_end_handler,
                              query_restore_fail_handler,
                              query_restore_after_child_handler);
  if(!restore_pr)
  {
      sixtp_destroy(top_level);
      return(NULL);
  }

  /* <query> <restore> <and-terms> */
  and_pr = sixtp_new();
  if (!and_pr) {
    sixtp_destroy(top_level);
    return (NULL);
  }
  sixtp_set_start(and_pr, query_and_start_handler);
  sixtp_set_chars(and_pr, allow_and_ignore_only_whitespace);
  sixtp_set_end(and_pr, query_and_end_handler);
  sixtp_set_fail(and_pr, query_and_fail_handler);
  sixtp_add_sub_parser(restore_pr, "and-terms", and_pr);
  
  RESTORE_PRED(datepred, "date-pred", qrestore);

  return(top_level);
}
