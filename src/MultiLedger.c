/*******************************************************************\
 * MultiLedger.c -- utilities for dealing with multiple             *
 * register/ledger windows in GnuCash                               *
 *                                                                  *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1997, 1998 Linas Vepstas                           *
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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
 *                                                                  *
\********************************************************************/

#include "config.h"

#include "Account.h"
#include "AccountP.h"
#include "Group.h"
#include "LedgerUtils.h"
#include "MultiLedger.h"
#include "Query.h"
#include "SplitLedger.h"
#include "Transaction.h"
#include "FileDialog.h"
#include "global-options.h"
#include "util.h"


/** GLOBALS *********************************************************/
/* These are globals because they describe the state of the entire
 * session. The is, there must be only one instance of these per GUI
 * session. */

static xaccLedgerDisplay **registerList = NULL; /* single-account registers */
static xaccLedgerDisplay **ledgerList = NULL;   /* multi-account registers */
static GList *fullList = NULL;    /* all registers */

static short module = MOD_LEDGER;


/********************************************************************\
 * Ledger utilities                                                 *
 * Although these seem like they might be replacable with stock     *
 * list handling calls, I want to leave them like this for now,     *
 * since they manipulate global variables.  If this code ever       *
 * gets multi-threaded, access and edit of these globals will have  *
 * to be controlled with mutexes, and these utility routines        *
 * present a rather natural place for the locks to be placed.       *
\********************************************************************/

/* ------------------------------------------------------ */

static GList *
ledgerListAdd (GList *list, xaccLedgerDisplay *ledger_display)
{
  if (ledger_display == NULL)
    return list;

  return g_list_prepend(list, ledger_display);
}

/* ------------------------------------------------------ */

static GList *
ledgerListRemove (GList *list, xaccLedgerDisplay *ledger_display)
{
  return g_list_remove(list, ledger_display);
}

/* ------------------------------------------------------ */

static gboolean
ledgerIsMember (xaccLedgerDisplay *ledger_display, Account * account)
{
  int n; 

  if (!account) return FALSE;
  if (!ledger_display) return FALSE;

  if (account == ledger_display->leader) return TRUE;

  /* Simple hack. Always return true for search registers. */
  if (ledger_display->type == SEARCH_LEDGER) return TRUE;

  if (! (ledger_display->displayed_accounts)) return FALSE;

  n = 0;
  while (ledger_display->displayed_accounts[n])
  {
    if (account == ledger_display->displayed_accounts[n])
      return TRUE;
    n++;
  }

  return FALSE;
}

/********************************************************************\
 * regWindowSimple                                                  *
 *   opens up a register window to display a single account         *
 *                                                                  *
 * Args:   acc     - the account associated with this register      *
 * Return: regData - the register window instance                   *
\********************************************************************/

xaccLedgerDisplay *
xaccLedgerDisplaySimple (Account *account)
{
  SplitRegisterType reg_type;
  GNCAccountType account_type;

  account_type = xaccAccountGetType (account);

  /* translate between different enumerants */
  switch (account_type) {
    case BANK:
      reg_type = BANK_REGISTER;
      break;
    case CASH:
      reg_type = CASH_REGISTER;
      break;
    case ASSET:
      reg_type = ASSET_REGISTER;
      break;
    case CREDIT:
      reg_type = CREDIT_REGISTER;
      break;
    case LIABILITY:
      reg_type = LIABILITY_REGISTER;
      break;
    case STOCK:
    case MUTUAL:
      reg_type = STOCK_REGISTER;
      break;
    case INCOME:
      reg_type = INCOME_REGISTER;
      break;
    case EXPENSE:
      reg_type = EXPENSE_REGISTER;
      break;
    case EQUITY:
      reg_type = EQUITY_REGISTER;
      break;
    case CURRENCY:
      reg_type = CURRENCY_REGISTER;
      break;
    default:
      PERR ("unknown account type %d\n", account_type);
      return NULL;
  }

  return xaccLedgerDisplayGeneral (account, NULL, reg_type, REG_SINGLE_LINE);
}

/********************************************************************\
 * xaccLedgerDisplayAccGroup                                        *
 *   opens up a register window to display an account, and all      *
 *   of its children, in the same window                            *
 *                                                                  *
 * Args:   account - the account associated with this register      *
 * Return: the register window instance                             *
\********************************************************************/

xaccLedgerDisplay *
xaccLedgerDisplayAccGroup (Account *account)
{
  SplitRegisterType ledger_type;
  xaccLedgerDisplay *retval;
  GNCAccountType account_type;
  GNCAccountType le_type;
  Account **list;
  Account *le;
  int n;

  /* build a flat list from the tree */
  list = xaccGroupToList (account);

  account_type = xaccAccountGetType (account);
  switch (account_type) {
    case BANK:
    case CASH:
    case ASSET:
    case CREDIT:
    case LIABILITY:
       /* if any of the sub-accounts have STOCK or MUTUAL types,
        * then we must use the PORTFOLIO_LEDGER ledger. Otherwise,
        * a plain old GENERAL_LEDGER will do. */
       ledger_type = GENERAL_LEDGER;

       le = list[0];
       n = 0;
       while (le) {
          le_type = xaccAccountGetType (le);
          if ((STOCK == le_type) || (MUTUAL == le_type)) {
             ledger_type = PORTFOLIO_LEDGER;
          }
          n++;
          le = list[n];
       }
       break;

    case STOCK:
    case MUTUAL:
       ledger_type = PORTFOLIO_LEDGER;
       break;

    case INCOME:
    case EXPENSE:
       ledger_type = INCOME_LEDGER;
       break;

    case EQUITY:
       ledger_type = GENERAL_LEDGER;
       break;

    default:
      PERR ("unknown account type \n");
      _free (list);
      return NULL;
  }

  retval = xaccLedgerDisplayGeneral (account, list, ledger_type,
                                     REG_SINGLE_LINE);

  if (list) _free (list);

  return retval;
}

static gncUIWidget
xaccLedgerDisplayParent(void *user_data)
{
  xaccLedgerDisplay *regData = user_data;

  if (regData == NULL)
    return NULL;

  if (regData->get_parent == NULL)
    return NULL;

  return (regData->get_parent)(regData);
}

static void
xaccLedgerDisplaySetHelp(void *user_data, const char *help_str)
{
  xaccLedgerDisplay *regData = user_data;

  if (regData == NULL)
    return;

  if (regData->set_help == NULL)
    return;

  (regData->set_help)(regData, help_str);
}

/********************************************************************\
 * xaccLedgerDisplayGeneral                                         *
 *   opens up a ledger window for a list of accounts                *
 *                                                                  *
 * Args:   lead_acc - the account associated with this register     *
 *                     (may be null)                                *
 *         acc_list - the list of accounts to display in register   *
 *                     (may be null)                                *
 * Return: regData  - the register window instance                  *
\********************************************************************/

xaccLedgerDisplay *
xaccLedgerDisplayGeneral (Account *lead_account, Account **accounts,
                          SplitRegisterType type, SplitRegisterStyle style)
{
  xaccLedgerDisplay *regData = NULL;
  gboolean show_all;

  /******************************************************************\
  \******************************************************************/

  /* the two macros below will search for a register windows associated
   * with the leading account.  If they exist, then they will return,
   * and that will be that.  If they do not exist, they will be created.
   *
   * There are two lists for lead-accounts: simple, single-account 
   * registers, which display one account only, and multiple-account
   * registers.  A leading account can have at most one of each. 
   * For a multiple-account register with a leader, all accounts
   * shown in the register are sub-accounts of the leader.
   *
   * A third possibility exists: a multiple-account register, with
   * no leader account.  In such a case, the list of accounts being
   * displayed have no particular relationship to each other.  There
   * can be an arbitrary number of multiple-account leader-less
   * registers.
   */
  regData = NULL;
  if (lead_account) {
     if (!accounts) {
       FETCH_FROM_LIST (xaccLedgerDisplay, registerList, lead_account, leader, regData);
     } else {
       FETCH_FROM_LIST (xaccLedgerDisplay, ledgerList, lead_account, leader, regData);
     }
  }

  /* if regData is null, then no leader account was specified */
  if (!regData) {
    regData = (xaccLedgerDisplay *) malloc (sizeof (xaccLedgerDisplay));
  }

  regData->leader = lead_account;
  regData->redraw = NULL;
  regData->destroy = NULL;
  regData->get_parent = NULL;
  regData->set_help = NULL;
  regData->gui_hook = NULL;
  regData->dirty = FALSE;
  regData->balance = 0.0;
  regData->clearedBalance = 0.0;
  regData->reconciledBalance = 0.0;

  /* count the number of accounts we are supposed to display,
   * and then, store them. */
  regData->numAcc = accListCount (accounts);
  regData->displayed_accounts = accListCopy (accounts);
  regData->type = type;

  show_all = gnc_lookup_boolean_option("Register",
                                       "Show All Transactions",
                                       TRUE);

  /* set up the query filter */
  regData->query = xaccMallocQuery();

  /* This is a bit of a hack. The number of splits should be
   * configurable, or maybe we should go back a time range instead
   * of picking a number, or maybe we should be able to exclude
   * based on reconciled status. Anyway, this works for now. */
  if (!show_all && (type != SEARCH_LEDGER))
    xaccQuerySetMaxSplits(regData->query, 30);

  xaccQuerySetGroup(regData->query, gncGetCurrentGroup());
  if (regData->displayed_accounts) {
    xaccQueryAddAccountMatch(regData->query, 
                             regData->displayed_accounts,
                             ACCT_MATCH_ANY, QUERY_OR);
  }
  if ((regData->leader != NULL) &&
      !accListHasAccount(regData->displayed_accounts, regData->leader)) {
    xaccQueryAddSingleAccountMatch(regData->query, regData->leader,
                                   QUERY_OR);
  }
  
  /* add this register to the list of registers */
  fullList = ledgerListAdd (fullList, regData);

  /******************************************************************\
   * The main register window itself                                *
  \******************************************************************/

  /* xaccMallocSplitRegister will malloc & initialize the register,
   * but will not do the gui init */
  regData->ledger = xaccMallocSplitRegister (type, style);

  xaccSRSetData(regData->ledger, regData,
                xaccLedgerDisplayParent,
                xaccLedgerDisplaySetHelp);

  regData->dirty = TRUE;
  xaccLedgerDisplayRefresh (regData);

  return regData;
}

/********************************************************************\
 * refresh only the indicated register window                       *
\********************************************************************/

void 
xaccLedgerDisplayRefresh (xaccLedgerDisplay *regData)
{
  /* If we don't really need the redraw, don't do it. */
  if (!(regData->dirty)) return;
  regData->dirty = FALSE;  /* mark clean */

  /* The leader account is used by the register gui to
   * assign a default source account for a "blank split"
   * that is attached to the bottom of the register.
   * The "blank split" is what the user edits to create 
   * new splits and get them into the system. */
  xaccSRLoadRegister (regData->ledger, 
                      xaccQueryGetSplits (regData->query),
                      regData->leader);

  /* hack alert -- this computation of totals is incorrect 
   * for multi-account ledgers */

  /* provide some convenience data for the the GUI window.
   * If the GUI wants to display yet other stuff, it's on its own. */
  regData->balance = xaccAccountGetBalance (regData->leader);
  regData->clearedBalance = xaccAccountGetClearedBalance (regData->leader);
  regData->reconciledBalance = 
    xaccAccountGetReconciledBalance(regData->leader);

  /* OK, now tell this specific GUI window to redraw itself ... */
  if (regData->redraw)
    (regData->redraw) (regData);
}

/********************************************************************\
 * refresh all the register windows, but only with the gui callback *
\********************************************************************/

void 
xaccRegisterRefreshAllGUI (void)
{
  xaccLedgerDisplay *ledger_display;
  GList *node;

  for (node = fullList; node; node = g_list_next(node))
  {
    ledger_display = node->data;
    if (ledger_display->redraw)
      (ledger_display->redraw) (ledger_display);
  }
}

/********************************************************************\
 * refresh only the indicated register window                       *
\********************************************************************/

void 
xaccRegisterRefresh (SplitRegister *splitreg)
{
  xaccLedgerDisplay *ledger_display;
  GList *node;

  for (node = fullList; node; node = g_list_next(node))
  {
    ledger_display = node->data;
    if (splitreg == ledger_display->ledger)
    {
      ledger_display->dirty = TRUE;
      xaccLedgerDisplayRefresh (ledger_display);
      return;
    }
  }
}

/********************************************************************\
 * mark dirty *all* register windows which contain this account      * 
\********************************************************************/

static void 
MarkDirtyAllRegs (Account *account)
{
  xaccLedgerDisplay *ledger_display;
  GList *node;

  for (node = fullList; node; node = g_list_next(node))
  {
    ledger_display = node->data;
    if (ledgerIsMember (ledger_display, account))
      ledger_display->dirty = TRUE;
  }
}

/********************************************************************\
 * refresh *all* register windows which contain this account        * 
\********************************************************************/

static void 
RefreshAllRegs (Account *account)
{
  xaccLedgerDisplay *ledger_display;
  GList *node;

  for (node = fullList; node; node = g_list_next(node))
  {
    ledger_display = node->data;
    if (ledgerIsMember (ledger_display, account))
      xaccLedgerDisplayRefresh (ledger_display);
  }
}

/********************************************************************\
\********************************************************************/

void 
xaccAccountDisplayRefresh (Account *acc)
{
  /* avoid excess screen flicker with a two-phase refresh */
  MarkDirtyAllRegs (acc);
  RefreshAllRegs (acc);
}

/********************************************************************\
\********************************************************************/

void
xaccAccGListDisplayRefresh (GList *accounts)
{
  GList *node;

  node = accounts;
  while (node) {
    MarkDirtyAllRegs (node->data);
    node = node->next;
  }

  node = accounts;
  while (node) {
    RefreshAllRegs (node->data);
    node = node->next;
  }
}

/********************************************************************\
\********************************************************************/

void 
xaccTransDisplayRefresh (Transaction *trans)
{
  int i, num_splits;

  /* avoid excess screen flicker with a two-phase refresh */
  num_splits = xaccTransCountSplits (trans);
  for (i=0; i<num_splits; i++) {
    Split *split = xaccTransGetSplit (trans, i);
    Account *account = xaccSplitGetAccount (split);
    MarkDirtyAllRegs (account);
  }
  for (i=0; i<num_splits; i++) {
    Split *split = xaccTransGetSplit (trans, i);
    Account *account = xaccSplitGetAccount (split);
    RefreshAllRegs (account);
  }
}

/********************************************************************\
 * xaccDestroyLedgerDisplay()
\********************************************************************/

void
xaccDestroyLedgerDisplay (Account *account)
{
  xaccLedgerDisplay *ledger_display;
  GList *close_list = NULL;
  GList *node;

  if (!account) return;

  /* find the single-account window for this account, if any */
  FIND_IN_LIST (xaccLedgerDisplay, registerList, account, leader,
                ledger_display);
  if (ledger_display)
  {
    if (ledger_display->destroy)
      (ledger_display->destroy) (ledger_display);
    xaccLedgerDisplayClose (ledger_display);
  }

  /* find the multiple-account window for this account, if any */
  FIND_IN_LIST (xaccLedgerDisplay, ledgerList, account, leader,
                ledger_display);
  if (ledger_display)
  {
    if (ledger_display->destroy)
      (ledger_display->destroy) (ledger_display);
    xaccLedgerDisplayClose (ledger_display);
  } 

  for (node = fullList; node; node = g_list_next(node))
  {
    ledger_display = node->data;
    if (ledgerIsMember (ledger_display, account))
      close_list = g_list_prepend (close_list, ledger_display);
  }

  for (node = close_list; node; node = g_list_next(node))
  {
    ledger_display = node->data;
    if (ledger_display->destroy)
      (ledger_display->destroy) (ledger_display);
    xaccLedgerDisplayClose (ledger_display);
  }

  g_list_free(close_list);
}

/********************************************************************\
 * xaccLedgerDisplayClose                                           *
 *   frees memory allocated for an regWindow, and other cleanup     *
 *   stuff                                                          *
 *                                                                  *
 * Args:   regData - ledger display structure                       *
 * Return: none                                                     *
\********************************************************************/
void 
xaccLedgerDisplayClose (xaccLedgerDisplay *ledger_display)
{
  Account *account;

  if (!ledger_display) return;

  account = ledger_display->leader;

  xaccDestroySplitRegister (ledger_display->ledger);

  /* whether this is a single or multi-account window, remove it */
  REMOVE_FROM_LIST (xaccLedgerDisplay, registerList, account, leader);
  REMOVE_FROM_LIST (xaccLedgerDisplay, ledgerList, account, leader);

  fullList = ledgerListRemove (fullList, ledger_display);

  xaccFreeQuery (ledger_display->query);

  free(ledger_display);
}

/************************** END OF FILE *************************/
