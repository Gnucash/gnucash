#include "config.h"

#include <string.h>

#include "sixtp.h"
#include "sixtp-utils.h"
#include "sixtp-parsers.h"

#include "Transaction.h"
#include "TransactionP.h"
#include "Group.h"

/****************************************************************************/
/* <transaction> (parent <ledger-data>)
 
   This block does nothing but pass the ledger-data account group down
   to its children.  It generates no data of its own, so it doesn't
   need any cleanup.

   input: AccountGroup*

   to-children-via-*result: AccountGroup*

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
transaction_start_handler(GSList* sibling_data,
                          gpointer parent_data,
                          gpointer global_data,
                          gpointer *data_for_children,
                          gpointer *result,
                          const gchar *tag)
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
 
   from parent: AccountGroup*

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
txn_restore_start_handler(GSList* sibling_data,
                          gpointer parent_data,
                          gpointer global_data,
                          gpointer *data_for_children,
                          gpointer *result,
                          const gchar *tag)
{
  Transaction *trans = xaccMallocTransaction();
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
  AccountGroup *ag = (AccountGroup *) parent_data;
  Transaction *trans = (Transaction *) data_for_children;

  g_return_val_if_fail(trans, FALSE);
  if(!ag) {
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
    if(trans->kvp_data) kvp_frame_delete(trans->kvp_data);
    trans->kvp_data = f;
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

  if(xaccTransLookup(&gid)) {
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

  xaccTransSetDateTS(t, &(info->ts));
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
txn_restore_split_start_handler(GSList* sibling_data,
                          gpointer parent_data,
                          gpointer global_data,
                          gpointer *data_for_children,
                          gpointer *result,
                          const gchar *tag)
{
  Split *s = xaccMallocSplit();
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
    if(s->kvp_data) kvp_frame_delete(s->kvp_data);
    s->kvp_data = f;
    child_result->should_cleanup = FALSE;
  }
  else if(strcmp(child_result->tag, "quantity") == 0) {
    gnc_numeric *n = (gnc_numeric *) child_result->data;
    g_return_val_if_fail(n, FALSE);
    xaccSplitSetShareAmount(s, *n);
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

  if(xaccSplitLookup(&gid)) {
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
  
  acct = xaccAccountLookup(&gid);
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
  sixtp *restore_pr;
  sixtp *damount_pr;
  sixtp *value_pr;
  sixtp *tmp_pr;
  
  top_level = sixtp_new();
  g_return_val_if_fail(top_level, NULL);
  sixtp_set_start(top_level, txn_restore_split_start_handler);
  sixtp_set_chars(top_level, allow_and_ignore_only_whitespace);
  sixtp_set_end(top_level, txn_restore_split_end_handler);
  sixtp_set_fail(top_level, txn_restore_split_fail_handler);
  sixtp_set_after_child(top_level, txn_restore_split_after_child_handler);

  /* <restore> (<guid> | <memo> | <action> | <account> | <reconcile-state>)  */
  restore_pr = top_level;
  sixtp_add_sub_parser(
      restore_pr, "guid",
      restore_char_generator(txn_restore_split_guid_end_handler));
  sixtp_add_sub_parser(
      restore_pr, "memo",
      restore_char_generator(txn_restore_split_memo_end_handler));
  sixtp_add_sub_parser(
      restore_pr, "action",
      restore_char_generator(txn_restore_split_action_end_handler));
  sixtp_add_sub_parser(
      restore_pr, "account",
      restore_char_generator(txn_restore_split_account_end_handler));
  sixtp_add_sub_parser(
      restore_pr, "reconcile-state",
      restore_char_generator(txn_restore_split_reconcile_state_end_handler));

  /* <restore> <reconcile-date> */
  sixtp_add_sub_parser(
      restore_pr, "reconcile-date", 
      generic_timespec_parser_new(txn_restore_split_reconcile_date_end_handler));
  
  /* <restore> <quantity> */
  damount_pr = generic_gnc_numeric_parser_new();
  if(!damount_pr) {
    sixtp_destroy(top_level);
    return(NULL);
  }
  sixtp_add_sub_parser(top_level, "quantity", damount_pr);

  /* <restore> <value> */
  value_pr = generic_gnc_numeric_parser_new();
  if(!value_pr) {
    sixtp_destroy(top_level);
    return(NULL);
  }
  sixtp_add_sub_parser(top_level, "value", value_pr);

  /* <restore> <slots> */
  tmp_pr = kvp_frame_parser_new();
  if(!tmp_pr) {
    sixtp_destroy(top_level);
    return(NULL);
  }
  sixtp_add_sub_parser(top_level, "slots", tmp_pr);

  return(top_level);
}

/***************************************************************************/

sixtp *
gnc_transaction_parser_new(void) 
{
  sixtp *top_level;
  sixtp *restore_pr;
  sixtp *split_pr;
  sixtp *tmp_pr;

  top_level = sixtp_new();
  g_return_val_if_fail(top_level, NULL);
  sixtp_set_start(top_level, transaction_start_handler);
  sixtp_set_chars(top_level, allow_and_ignore_only_whitespace);
  sixtp_set_after_child(top_level, txn_restore_after_child_handler);

  /* <restore> */
  restore_pr = setup_restorer(top_level,
                              txn_restore_start_handler,
                              txn_restore_end_handler,
                              txn_restore_fail_handler,
                              txn_restore_after_child_handler);
  if(!restore_pr)
  {
      sixtp_destroy(top_level);
      return(NULL);
  }

  /* <restore> (<guid> | <num> | <description> ) */
  sixtp_add_sub_parser(
      restore_pr, "guid",
      restore_char_generator(txn_restore_guid_end_handler));
  sixtp_add_sub_parser(
      restore_pr, "num",
      restore_char_generator(txn_restore_num_end_handler));
  sixtp_add_sub_parser(
      restore_pr, "description",
      restore_char_generator(txn_restore_description_end_handler));

  /* <restore> (<date-posted> | <date-entered>) */
  sixtp_add_sub_parser(
      restore_pr, "date-posted", 
      generic_timespec_parser_new(txn_rest_date_posted_end_handler));
  sixtp_add_sub_parser(
      restore_pr, "date-entered", 
      generic_timespec_parser_new(txn_rest_date_entered_end_handler));
  
  /* <restore> <slots> */
  tmp_pr = kvp_frame_parser_new();
  if(!tmp_pr) {
    sixtp_destroy(top_level);
    return(NULL);
  }
  sixtp_add_sub_parser(restore_pr, "slots", tmp_pr);

  /* <restore> <split> */
  split_pr = gnc_txn_restore_split_parser_new();
  if(!split_pr) {
    sixtp_destroy(top_level);
    return(NULL);
  }
  sixtp_add_sub_parser(restore_pr, "split", split_pr);

  return(top_level);
}
