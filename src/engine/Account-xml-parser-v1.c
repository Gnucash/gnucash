#include "config.h"

#include <string.h>

#include "sixtp.h"
#include "sixtp-utils.h"
#include "sixtp-parsers.h"

#include "Account.h"
#include "AccountP.h"
#include "Group.h"

/****************************************************************************/
/* <account> (parent <ledger-data>)
 
   This block does nothing but pass the ledger-data account group down
   to its children.  It generates no data of its own, so it doesn't
   need any cleanup.

   input: AccountGroup*

   to-children-via-*result: AccountGroup*

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
 
   input: AccountGroup*
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
  Account *acc = xaccMallocAccount();
  
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
  AccountGroup *ag = (AccountGroup *) parent_data;
  Account *acc = (Account *) *result;
  AccountGroup *parent_ag;

  g_return_val_if_fail((ag && acc), FALSE);

  /* CHECKME: do we need to xaccAccountRecomputeBalance(acc) here? */
  xaccAccountCommitEdit(acc);

  /* If the account doesn't have a parent yet, just cram it into the
     top level */
  parent_ag = xaccAccountGetParent(acc);

  if(!parent_ag) xaccGroupInsertAccount(ag, acc);

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
  g_return_val_if_fail(a, FALSE);
  if(!child_result) return(TRUE);
  if(child_result->type != SIXTP_CHILD_RESULT_NODE) return(TRUE);
  if(strcmp(child_result->tag, "slots") == 0) {
    kvp_frame *f = (kvp_frame *) child_result->data;
    g_return_val_if_fail(f, FALSE);
    if(a->kvp_data) kvp_frame_delete(a->kvp_data);
    a->kvp_data = f;
    child_result->should_cleanup = FALSE;
  }
  else if(strcmp(child_result->tag, "currency") == 0) {
    gnc_commodity *com = (gnc_commodity *) child_result->data;
    g_return_val_if_fail(com, FALSE);
    if(xaccAccountGetCurrency(a)) return FALSE;
    xaccAccountSetCurrency(a, com);
    /* let the normal child_result handler clean up com */
  }
  else if(strcmp(child_result->tag, "security") == 0) {
    gnc_commodity *com = (gnc_commodity *) child_result->data;
    g_return_val_if_fail(com, FALSE);
    if(xaccAccountGetSecurity(a)) return FALSE;
    xaccAccountSetSecurity(a, com);
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
  if(acc) xaccFreeAccount(acc);
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

  if(xaccAccountLookup(&gid)) {
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

  parent = xaccAccountLookup(&gid);
  
  g_return_val_if_fail(parent, FALSE);

  xaccRemoveAccount(acc); /* just to be anal */
  xaccAccountInsertSubAccount(parent, acc);

  return(TRUE);
}

sixtp *
gnc_account_parser_new(void)
{
  sixtp *restore_pr;
  sixtp *acc_restore_currency_pr;
  sixtp *acc_restore_security_pr;
  sixtp *acc_restore_parent_pr;
  sixtp *acc_restore_parent_guid_pr;
  sixtp *acc_restore_slots_pr;
  sixtp *ret;
    
  /* <account> */
  ret = sixtp_new();
  g_return_val_if_fail(ret, NULL);

  sixtp_set_start(ret, account_start_handler);
  sixtp_set_chars(ret, allow_and_ignore_only_whitespace);
  
  /* <account> <restore> */
  restore_pr = setup_restorer(ret, account_restore_start_handler,
                              account_restore_end_handler,
                              account_restore_fail_handler,
                              account_restore_after_child_handler);
  g_return_val_if_fail(restore_pr, NULL);
  
  /* <restore> (<name> | <guid> | <type> | <code> | <description> | <notes>)*/
  sixtp_add_sub_parser(
      restore_pr, "name",
      restore_char_generator(acc_restore_name_end_handler));
  sixtp_add_sub_parser(
      restore_pr, "guid",
      restore_char_generator(acc_restore_guid_end_handler));
  sixtp_add_sub_parser(
      restore_pr, "type",
      restore_char_generator(acc_restore_type_end_handler));
  sixtp_add_sub_parser(
      restore_pr, "code",
      restore_char_generator(acc_restore_code_end_handler));
  sixtp_add_sub_parser(
      restore_pr, "description",
      restore_char_generator(acc_restore_description_end_handler));
  sixtp_add_sub_parser(
      restore_pr, "notes",
      restore_char_generator(acc_restore_notes_end_handler));
  
  /* <account> <restore> <currency> */
  acc_restore_currency_pr = generic_gnc_commodity_lookup_parser_new();
  g_return_val_if_fail(acc_restore_currency_pr, NULL);
  sixtp_add_sub_parser(restore_pr, "currency", acc_restore_currency_pr);
  
  /* <account> <restore> <security> */
  acc_restore_security_pr = generic_gnc_commodity_lookup_parser_new();
  g_return_val_if_fail(acc_restore_security_pr, NULL);
  sixtp_add_sub_parser(restore_pr, "security", acc_restore_security_pr);

  /* <account> <restore> <parent> */
  acc_restore_parent_pr = sixtp_new();
  g_return_val_if_fail(acc_restore_parent_pr, NULL);
  sixtp_set_chars(acc_restore_parent_pr, allow_and_ignore_only_whitespace);
  sixtp_set_end(acc_restore_parent_pr, acc_restore_parent_end_handler);
  sixtp_add_sub_parser(restore_pr, "parent", acc_restore_parent_pr);
  
  /* <account> <restore> <parent> <guid> */
  acc_restore_parent_guid_pr = generic_guid_parser_new();
  g_return_val_if_fail(acc_restore_parent_guid_pr, NULL);
  sixtp_add_sub_parser(acc_restore_parent_pr, "guid",
                       acc_restore_parent_guid_pr);
  
  /* <account> <restore> <slots> */
  acc_restore_slots_pr = kvp_frame_parser_new();
  g_return_val_if_fail(acc_restore_slots_pr, NULL);
  sixtp_add_sub_parser(restore_pr, "slots", acc_restore_slots_pr);

  return ret;
}
