
#include "sixtp.h"

#include "sixtp-parsers.h"
#include "sixtp-utils.h"

#include "Group.h"
#include "TransLog.h"

/****************************************************************************/
/****************************************************************************/
/****************************************************************************/
/* <ledger-data> (parent <gnc-data>)

   On failure or on normal cleanup, the account group will be killed,
   so if you want it, you better set should_cleanup to false

   input: NA
   to-children-via-*result: new AccountGroup*
   returns: an AccountGroup*
   start: creates the account group and puts it into *result
   characters: NA
   end: finishes up the account group and leaves it in result.
   cleanup-result: deletes the account group (use should_cleanup to avoid).
   cleanup-chars: NA
   fail: deletes the account group in *result.
   result-fail: same as cleanup-result.
   chars-fail: NA

*/


static gboolean
ledger_data_start_handler(GSList* sibling_data,
                          gpointer parent_data,
                          gpointer global_data,
                          gpointer *data_for_children,
                          gpointer *result,
                          const gchar *tag)
{
  AccountGroup *ag;

  /* disable logging during load; otherwise its just a mess */
  xaccLogDisable();
  ag = xaccMallocAccountGroup();

  g_return_val_if_fail(ag, FALSE);

  *data_for_children = ag;
  return(ag != NULL);
}

static gboolean
ledger_data_end_handler(gpointer data_for_children,
                        GSList  *data_from_children, GSList *sibling_data,
                        gpointer parent_data, gpointer global_data,
                        gpointer *result, const gchar *tag)
{
  
  AccountGroup *ag = (AccountGroup *) data_for_children;

  g_return_val_if_fail(ag, FALSE);

  /* mark the newly read group as saved, since the act of putting 
   * it together will have caused it to be marked up as not-saved. 
   */
  xaccGroupMarkSaved (ag);

  /* auto-number the accounts, if they are not already numbered */
  xaccGroupDepthAutoCode (ag);

  /* commit all groups, this completes the BeginEdit started when the
   * account_end_handler finished reading the account.
   */
  xaccAccountGroupCommitEdit (ag);

  /* set up various state that is not normally stored in the byte stream */
  xaccRecomputeGroupBalance (ag);

  xaccLogEnable();

  *result = ag;
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
  AccountGroup *ag = (AccountGroup *) data_for_children;
  if(ag) xaccFreeAccountGroup(ag);
}

static void
ledger_data_result_cleanup(sixtp_child_result *cr)
{
  AccountGroup *ag = (AccountGroup *) cr->data;
  if(ag) xaccFreeAccountGroup(ag);
}


sixtp*
ledger_data_parser_new(void) 
{
  sixtp *top_level;
  sixtp *acc_pr;
  sixtp *com_pr;
  sixtp *tran_pr;
  
  /* <ledger-data> */
  top_level = sixtp_new();
  g_return_val_if_fail(top_level, NULL);

  sixtp_set_start(top_level, ledger_data_start_handler);
  sixtp_set_chars(top_level, allow_and_ignore_only_whitespace);
  sixtp_set_end(top_level, ledger_data_end_handler);
  sixtp_set_cleanup_result(top_level, ledger_data_result_cleanup);
  sixtp_set_fail(top_level, ledger_data_fail_handler);
  sixtp_set_result_fail(top_level, ledger_data_result_cleanup);

  /* <commodity> */
  com_pr = commodity_restore_parser_new();
  if(!com_pr) {
    sixtp_destroy(top_level);
    return(NULL);
  }
  sixtp_add_sub_parser(top_level, "commodity", com_pr);  

  acc_pr = gnc_account_parser_new();
  if(!acc_pr) {
    sixtp_destroy(top_level);
    return(NULL);
  }
  sixtp_add_sub_parser(top_level, "account", acc_pr);
  
  /* <transaction> */
  tran_pr = gnc_transaction_parser_new();
  if(!tran_pr) {
    sixtp_destroy(top_level);
    return(NULL);
  }
  sixtp_add_sub_parser(top_level, "transaction", tran_pr);

  return(top_level);
}
