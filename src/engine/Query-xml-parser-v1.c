#include <glib.h>

#include <string.h>

#include "sixtp.h"
#include "sixtp-utils.h"
#include "sixtp-parsers.h"
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


/***********************************************************************/
/***********************************************************************/
/* WRITING */
/* push query terms into xml */
/* XXX hack alert not all predicates currently implemented */

static gboolean
xml_add_qterm_restorer(xmlNodePtr qxml, QueryTerm *qt) 
{
  int rc;
  xmlNodePtr p = NULL;

  g_return_val_if_fail(qxml, FALSE);
  g_return_val_if_fail(qt, FALSE);

  /* we set the predicates names based on the info they record */
  switch (qt->data.base.term_type) {
    case PR_ACCOUNT: 
       p = xmlNewTextChild(qxml, NULL, "account-pred", NULL);  
       break;

    case PR_ACTION: 
       p = xmlNewTextChild(qxml, NULL, "action-pred", NULL);  
       break;

    case PR_AMOUNT:
       p = xmlNewTextChild(qxml, NULL, "amount-pred", NULL);  
       break;

    case PR_BALANCE:
       p = xmlNewTextChild(qxml, NULL, "balance-pred", NULL);  
       break;

    case PR_CLEARED:
       p = xmlNewTextChild(qxml, NULL, "cleared-pred", NULL);  
       break;

    case PR_DATE:
       p = xmlNewTextChild(qxml, NULL, "date-pred", NULL);  
       break;

    case PR_DESC:
       p = xmlNewTextChild(qxml, NULL, "description-pred", NULL);  
       break;

    case PR_MEMO:
       p = xmlNewTextChild(qxml, NULL, "memo-pred", NULL);  
       break;

    case PR_NUM:
       p = xmlNewTextChild(qxml, NULL, "num-pred", NULL);  
       break;

    case PR_PRICE:
       p = xmlNewTextChild(qxml, NULL, "price-pred", NULL);  
       break;

    case PR_SHRS:
       p = xmlNewTextChild(qxml, NULL, "shares-pred", NULL);  
       break;

    case PR_MISC:
       PERR ("Misc terms are not transmittable");
       break;

    default:
  }
  if (!p) return (FALSE);

  rc = xml_add_gint32(p, "sense", qt->data.base.sense);
  if (!rc) return(FALSE);


  /* however, many of the types share a generic structure. */
  switch (qt->data.type) {
    case PD_ACCOUNT: 
       PERR ("account query unimplemented");
       break;

    case PD_AMOUNT:
       PERR ("amount query unimplemented");
       break;

    case PD_BALANCE:
       PERR ("balance query unimplemented");
       break;

    case PD_CLEARED:
       PERR ("cleared query unimplemented");
       break;

    case PD_DATE:
       xml_add_gint32(p, "use-start", qt->data.date.use_start);
       xml_add_gint32(p, "use-end", qt->data.date.use_end);
       if (qt->data.date.use_start) {
          xml_add_editable_timespec(p, "start-date", 
                                   &(qt->data.date.start), FALSE);
       }
       if (qt->data.date.use_end) {
          xml_add_editable_timespec(p, "end-date", 
                                   &(qt->data.date.end), FALSE);
       }
       break;

    case PD_STRING:
       xml_add_gint32(p, "case-sens", qt->data.str.case_sens);
       xml_add_gint32(p, "use-regexp", qt->data.str.use_regexp);
       xml_add_str(p, "matchstring", qt->data.str.matchstring, TRUE);
       break;

    case PD_MISC:
       PERR ("Must not happen");
       break;

    default:
  }

  return(TRUE);
}

/* ============================================================== */
/* loop over all terms in the query */
/* XXX hack alert --  need to also send max-terms, sort-order,
 * and other mis query elements */

gboolean
xml_add_query_restorers(xmlNodePtr p, Query *q) 
{
  xmlNodePtr qxml, restore_xml, and_xml;
  GList *aterms, *oterms;
  GList *anode, *onode;
  
  g_return_val_if_fail(p, FALSE);
  g_return_val_if_fail(q, FALSE);

  oterms = xaccQueryGetTerms (q);

  /* write the nested <query> <restore> */
  qxml = xmlNewTextChild(p, NULL, "query", NULL);  
  g_return_val_if_fail(qxml, FALSE);

  restore_xml = xmlNewTextChild(qxml, NULL, "restore", NULL);  
  g_return_val_if_fail(restore_xml, FALSE);

  for (onode = oterms; onode; onode = onode->next) {
    aterms = onode->data;
    and_xml = xmlNewTextChild(restore_xml, NULL, "and-terms", NULL);  
    g_return_val_if_fail(and_xml, FALSE);

    for (anode = aterms; anode; anode = anode->next) {
      QueryTerm *qt = anode->data;
      xml_add_qterm_restorer(and_xml, qt);
    }
  }
  return(TRUE);
}
