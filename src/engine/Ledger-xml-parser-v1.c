/********************************************************************\
 * Ledger-xml-parser-v1.c                                           *
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

#include <string.h>

#include "gnc-engine-util.h"
#include "gnc-pricedb-p.h"
#include "io-gncxml-p.h"
#include "sixtp.h"
#include "sixtp-parsers.h"
#include "sixtp-utils.h"

#include "Group.h"
#include "TransLog.h"


/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_IO;

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
ledger_data_start_handler(GSList* sibling_data, gpointer parent_data,
                          gpointer global_data, gpointer *data_for_children,
                          gpointer *result, const gchar *tag, gchar **attrs)
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
    gnc_pricedb_mark_clean(pdb);
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
  
  AccountGroup *ag = (AccountGroup *) data_for_children;

  g_return_val_if_fail(ag, FALSE);

  /* mark the newly read group as saved, since the act of putting 
   * it together will have caused it to be marked up as not-saved. 
   */
  xaccGroupMarkSaved (ag);

  /* commit all groups, this completes the BeginEdit started when the
   * account_end_handler finished reading the account.
   */
  xaccAccountGroupCommitEdit (ag);

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
