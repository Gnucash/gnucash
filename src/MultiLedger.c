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
#include "Group.h"
#include "MultiLedger.h"
#include "Query.h"
#include "SplitLedger.h"
#include "Transaction.h"
#include "FileDialog.h"
#include "global-options.h"
#include "gnc-component-manager.h"
#include "gnc-engine-util.h"


#define REGISTER_SINGLE_CM_CLASS     "register-single"
#define REGISTER_SUBACCOUNT_CM_CLASS "register-subaccount"
#define REGISTER_GL_CM_CLASS         "register-gl"

/** GLOBALS *********************************************************/
static short module = MOD_LEDGER;


/** Declarations ****************************************************/
static xaccLedgerDisplay *
xaccLedgerDisplayInternal (Account *lead_account, GList *accounts, Query *q,
                           SplitRegisterType type, SplitRegisterStyle style);


/** Implementations *************************************************/

static gboolean
find_by_account (gpointer find_data, gpointer user_data)
{
  Account *account = find_data;
  xaccLedgerDisplay *ld = user_data;

  if (!account || !ld)
    return FALSE;

  if (account == ld->leader)
    return TRUE;

  if (ld->type < NUM_SINGLE_REGISTER_TYPES)
    return FALSE;

  if (ld->displayed_accounts)
    return g_list_find (ld->displayed_accounts, account) != NULL;

  /* Hack. */
  return TRUE;
}

static SplitRegisterStyle
gnc_get_default_register_style ()
{
  SplitRegisterStyle new_style = REG_STYLE_LEDGER;
  char *style_string;

  style_string = gnc_lookup_multichoice_option("Register", 
                                               "Default Register Style",
                                               "ledger");

  if (safe_strcmp(style_string, "ledger") == 0)
    new_style = REG_STYLE_LEDGER;
  else if (safe_strcmp(style_string, "auto_ledger") == 0)
    new_style = REG_STYLE_AUTO_LEDGER;
  else if (safe_strcmp(style_string, "journal") == 0)
    new_style = REG_STYLE_JOURNAL;

  if (style_string != NULL)
    free(style_string);

  return new_style;
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
  switch (account_type)
  {
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

  return xaccLedgerDisplayGeneral (account, NULL, reg_type,
                                   gnc_get_default_register_style ());
}

static GList *
xaccAccountPrependChildren (Account *account, GList *list)
{
  AccountGroup *group;
  int num_accounts;
  int i;

  if (!account) return NULL;

  list = g_list_prepend(list, account);

  group = xaccAccountGetChildren (account);
  if (group == NULL)
    return list;

  num_accounts = xaccGroupGetNumAccounts (group);
  for (i = 0; i < num_accounts; i++)
  {
    account = xaccGroupGetAccount (group, i);
    list = xaccAccountPrependChildren (account, list);
  }

  return list;
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
  xaccLedgerDisplay *ld;
  SplitRegisterType reg_type;
  GNCAccountType le_type;
  GList *accounts;
  GList *node;

  /* build a flat list from the tree */
  accounts = xaccAccountPrependChildren (account, NULL);
  accounts = g_list_reverse (accounts);

  switch (xaccAccountGetType (account))
  {
    case BANK:
    case CASH:
    case ASSET:
    case CREDIT:
    case LIABILITY:
      /* if any of the sub-accounts have STOCK or MUTUAL types,
       * then we must use the PORTFOLIO_LEDGER ledger. Otherwise,
       * a plain old GENERAL_LEDGER will do. */
      reg_type = GENERAL_LEDGER;

      for (node = accounts; node; node = node->next)
      {
        le_type = xaccAccountGetType (node->data);
        if ((STOCK == le_type) || (MUTUAL == le_type))
        {
          reg_type = PORTFOLIO_LEDGER;
          break;
        }
      }
      break;

    case STOCK:
    case MUTUAL:
      reg_type = PORTFOLIO_LEDGER;
      break;

    case INCOME:
    case EXPENSE:
      reg_type = INCOME_LEDGER;
      break;

    case EQUITY:
      reg_type = GENERAL_LEDGER;
      break;

    default:
      PERR ("unknown account type \n");
      g_list_free (accounts);
      return NULL;
  }

  ld = xaccLedgerDisplayGeneral (account, accounts, reg_type,
                                 REG_STYLE_JOURNAL);

  g_list_free (accounts);

  return ld;
}

static gncUIWidget
xaccLedgerDisplayParent (void *user_data)
{
  xaccLedgerDisplay *regData = user_data;

  if (regData == NULL)
    return NULL;

  if (regData->get_parent == NULL)
    return NULL;

  return regData->get_parent (regData);
}

static void
xaccLedgerDisplaySetHelp (void *user_data, const char *help_str)
{
  xaccLedgerDisplay *regData = user_data;

  if (regData == NULL)
    return;

  if (regData->set_help == NULL)
    return;

  regData->set_help (regData, help_str);
}

static gpointer
xaccGUIDMalloc (void)
{
  GUID *guid;

  guid = g_new(GUID, 1);

  *guid = *xaccGUIDNULL();

  return guid;
}

static void
xaccGUIDFree (gpointer _guid)
{
  GUID *guid = _guid;

  if (guid == NULL)
    return;

  *guid = *xaccGUIDNULL();

  g_free(guid);
}

static void
xaccGUIDCopy (gpointer _to, gconstpointer _from)
{
  GUID *to = _to;
  const GUID *from = _from;

  g_return_if_fail(to != NULL);

  if (from == NULL)
    *to = *xaccGUIDNULL();
  else
    *to = *from;
}

static void
close_handler (gpointer user_data)
{
  xaccLedgerDisplay *ld = user_data;

  if (!ld)
    return;

  gnc_unregister_gui_component (ld->component_id);

  if (ld->destroy)
      ld->destroy (ld);

  xaccDestroySplitRegister (ld->reg);
  ld->reg = NULL;

  xaccFreeQuery (ld->query);
  ld->query = NULL;

  g_list_free (ld->displayed_accounts);
  ld->displayed_accounts = NULL;

  g_free (ld);
}

static void
make_ledger_query (xaccLedgerDisplay *ld, gboolean show_all,
                   SplitRegisterType type)
{
  ld->query = xaccMallocQuery ();

  /* This is a bit of a hack. The number of splits should be
   * configurable, or maybe we should go back a time range instead
   * of picking a number, or maybe we should be able to exclude
   * based on reconciled status. Anyway, this works for now. */
  if (!show_all && (type != SEARCH_LEDGER))
    xaccQuerySetMaxSplits (ld->query, 30);

  xaccQuerySetGroup (ld->query, gncGetCurrentGroup());

  if (ld->displayed_accounts)
    xaccQueryAddAccountMatch (ld->query, ld->displayed_accounts,
                              ACCT_MATCH_ANY, QUERY_OR);

  if (ld->leader &&
      (g_list_find (ld->displayed_accounts, ld->leader) == NULL))
    xaccQueryAddSingleAccountMatch (ld->query, ld->leader, QUERY_OR);
}

/********************************************************************\
 * xaccLedgerDisplayGeneral                                         *
 *   opens up a ledger window for a list of accounts                *
 *                                                                  *
 * Args:   lead_account - the account associated with this register *
 *                        (may be NULL)                             *
 *         accounts     - the list of accounts to display           *
 *                        (may be NULL)                             *
 *         type         - the type of split register to open        *
 *         style        - the style of register to use              *
 * Return: the register window instance                             *
\********************************************************************/
xaccLedgerDisplay *
xaccLedgerDisplayGeneral (Account *lead_account, GList *accounts,
                          SplitRegisterType type, SplitRegisterStyle style)
{
  return xaccLedgerDisplayInternal (lead_account, accounts, NULL, type, style);
}

/********************************************************************\
 * xaccLedgerDisplayQuery                                           *
 *   opens up a ledger window for an arbitrary query                *
 *                                                                  *
 * Args:   query - the query to use for the register                *
 *         type  - the type of split register to open               *
 *         style - the style of register to use                     *
 * Return: the register window instance                             *
\********************************************************************/
xaccLedgerDisplay *
xaccLedgerDisplayQuery (Query *query, SplitRegisterType type,
                        SplitRegisterStyle style)
{
  return xaccLedgerDisplayInternal (NULL, NULL, query, type, style);
}

void
xaccLedgerDisplaySetQuery (xaccLedgerDisplay *ledger_display, Query *q)
{
  if (!ledger_display || !q)
    return;

  xaccFreeQuery (ledger_display->query);
  ledger_display->query = xaccQueryCopy (q);
}

static xaccLedgerDisplay *
xaccLedgerDisplayInternal (Account *lead_account, GList *accounts, Query *q,
                           SplitRegisterType type, SplitRegisterStyle style)
{
  xaccLedgerDisplay *ld;
  gboolean show_all;
  const char *class;

  if (type < NUM_SINGLE_REGISTER_TYPES) /* single account types */
  {
    if (!lead_account)
    {
      PERR ("single-account register with no account specified");
      return NULL;
    }

    class = REGISTER_SINGLE_CM_CLASS;

    ld = gnc_find_first_gui_component (class, find_by_account, lead_account);

    if (ld)
      return ld;
  }
  else if (lead_account) /* sub-account registers */
  {
    class = REGISTER_SUBACCOUNT_CM_CLASS;

    ld = gnc_find_first_gui_component (class, find_by_account, lead_account);

    if (ld)
      return ld;
  }
  else
    class = REGISTER_GL_CM_CLASS;

  ld = g_new (xaccLedgerDisplay, 1);

  ld->type = type;
  ld->leader = lead_account;
  ld->destroy = NULL;
  ld->get_parent = NULL;
  ld->set_help = NULL;
  ld->gui_hook = NULL;
  ld->dirty = FALSE;

  /* store the displayed accounts */
  ld->displayed_accounts = g_list_copy (accounts);

  show_all = gnc_lookup_boolean_option ("Register",
                                        "Show All Transactions",
                                        TRUE);

  /* set up the query filter */
  if (q)
    ld->query = xaccQueryCopy (q);
  else
    make_ledger_query (ld, show_all, type);

  ld->component_id = gnc_register_gui_component (class, NULL,
                                                 close_handler, ld);

  /******************************************************************\
   * The main register window itself                                *
  \******************************************************************/

  /* xaccMallocSplitRegister will malloc & initialize the register,
   * but will not do the gui init */
  ld->reg = xaccMallocSplitRegister (type, style, FALSE,
                                     xaccSRGetEntryHandler,
                                     xaccSRGetLabelHandler,
                                     xaccSRGetIOFlagsHandler,
                                     xaccSRGetFGColorHandler,
                                     xaccSRGetBGColorHandler,
                                     xaccSRGetCellBorderHandler,
                                     xaccGUIDMalloc,
                                     xaccGUIDFree,
                                     xaccGUIDCopy);

  xaccSRSetData (ld->reg, ld,
                 xaccLedgerDisplayParent,
                 xaccLedgerDisplaySetHelp);

  ld->dirty = TRUE;
  xaccLedgerDisplayRefresh (ld);

  return ld;
}

/********************************************************************\
 * refresh only the indicated register window                       *
\********************************************************************/

void 
xaccLedgerDisplayRefresh (xaccLedgerDisplay *ld)
{
  /* If we don't really need the redraw, don't do it. */
  if (!ld->dirty)
    return;

  ld->dirty = FALSE;  /* mark clean */

  /* The leader account is used by the register gui to
   * assign a default source account for a "blank split"
   * that is attached to the bottom of the register.
   * The "blank split" is what the user edits to create 
   * new splits and get them into the system. */
  xaccSRLoadRegister (ld->reg, xaccQueryGetSplits (ld->query), ld->leader);
}

/********************************************************************\
 * mark dirty *all* register windows which contain this account      * 
\********************************************************************/

static void 
MarkDirtyAllRegsClass (Account *account, const char *component_class)
{
  GList *list;
  GList *node;

  list = gnc_find_gui_components (component_class, find_by_account, account);

  for (node = list; node; node = node->next)
  {
    xaccLedgerDisplay *ld = node->data;

    ld->dirty = TRUE;
  }
}

static void
MarkDirtyAllRegs (Account *account)
{
  MarkDirtyAllRegsClass (account, REGISTER_SINGLE_CM_CLASS);
  MarkDirtyAllRegsClass (account, REGISTER_SUBACCOUNT_CM_CLASS);
  MarkDirtyAllRegsClass (account, REGISTER_GL_CM_CLASS);
}

/********************************************************************\
 * refresh *all* register windows which contain this account        * 
\********************************************************************/

static void 
RefreshAllRegsClass (Account *account, const char *component_class)
{
  GList *list;
  GList *node;

  list = gnc_find_gui_components (component_class, find_by_account, account);

  for (node = list; node; node = node->next)
  {
    xaccLedgerDisplay *ld = node->data;

    xaccLedgerDisplayRefresh (ld);
  }
}

static void 
RefreshAllRegs (Account *account)
{
  RefreshAllRegsClass (account, REGISTER_SINGLE_CM_CLASS);
  RefreshAllRegsClass (account, REGISTER_SUBACCOUNT_CM_CLASS);
  RefreshAllRegsClass (account, REGISTER_GL_CM_CLASS);
}

/********************************************************************\
\********************************************************************/

void
xaccAccountDisplayRefresh (Account *account)
{
  if (!account)
    return;

  MarkDirtyAllRegs (account);
  RefreshAllRegs   (account);
}

/********************************************************************\
\********************************************************************/

void
xaccAccGListDisplayRefresh (GList *accounts)
{
  GList *node;

  for (node = accounts; node; node = node->next)
    MarkDirtyAllRegs (node->data);

  for (node = accounts; node; node = node->next)
    RefreshAllRegs (node->data);
}

/********************************************************************\
\********************************************************************/

void 
xaccTransDisplayRefresh (Transaction *trans)
{
  GList *node;

  for (node = xaccTransGetSplitList (trans); node; node = node->next)
  {
    Split *split = node->data;
    Account *account = xaccSplitGetAccount (split);

    if (account)
      MarkDirtyAllRegs (account);
  }

  for (node = xaccTransGetSplitList (trans); node; node = node->next)
  {
    Split *split = node->data;
    Account *account = xaccSplitGetAccount (split);

    if (account)
      RefreshAllRegs (account);
  }
}

/********************************************************************\
 * xaccDestroyLedgerDisplay()
\********************************************************************/

static void
xaccDestroyLedgerDisplayClass (Account *account, const char *component_class)
{
  GList *list;
  GList *node;

  list = gnc_find_gui_components (component_class, find_by_account, account);

  for (node = list; node; node = node->next)
  {
    xaccLedgerDisplay *ld = node->data;

    gnc_close_gui_component (ld->component_id);
  }
}

void
xaccDestroyLedgerDisplay (Account *account)
{
  if (!account)
    return;

  xaccDestroyLedgerDisplayClass (account, REGISTER_SINGLE_CM_CLASS);
  xaccDestroyLedgerDisplayClass (account, REGISTER_SUBACCOUNT_CM_CLASS);
  xaccDestroyLedgerDisplayClass (account, REGISTER_GL_CM_CLASS);
}

void
xaccLedgerDisplayClose (xaccLedgerDisplay *ld)
{
  if (!ld)
    return;

  gnc_close_gui_component (ld->component_id);
}

/************************** END OF FILE *************************/
