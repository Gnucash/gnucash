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


struct _xaccLedgerDisplay
{
  GUID leader;

  Query *query;

  LedgerDisplayType ld_type;

  SplitRegister *reg;

  LedgerDisplayDestroy destroy;
  LedgerDisplayGetParent get_parent;
  LedgerDisplaySetHelp set_help;

  gpointer user_data;

  gint component_id;
};


/** GLOBALS *********************************************************/
static short module = MOD_LEDGER;


/** Declarations ****************************************************/
static xaccLedgerDisplay *
xaccLedgerDisplayInternal (Account *lead_account, Query *q,
                           LedgerDisplayType ld_type,
                           SplitRegisterType reg_type,
                           SplitRegisterStyle style);


/** Implementations *************************************************/

Account *
xaccLedgerDisplayLeader (xaccLedgerDisplay *ld)
{
  if (!ld)
    return NULL;

  return xaccAccountLookup (&ld->leader);
}

void
xaccLedgerDisplaySetUserData (xaccLedgerDisplay *ld, gpointer user_data)
{
  if (!ld)
    return;

  ld->user_data = user_data;
}

gpointer
xaccLedgerDisplayGetUserData (xaccLedgerDisplay *ld)
{
  if (!ld)
    return NULL;

  return ld->user_data;
}

void
xaccLedgerDisplaySetHandlers (xaccLedgerDisplay *ld,
                              LedgerDisplayDestroy destroy,
                              LedgerDisplayGetParent get_parent,
                              LedgerDisplaySetHelp set_help)
{
  if (!ld)
    return;

  ld->destroy = destroy;
  ld->get_parent = get_parent;
  ld->set_help = set_help;
}

SplitRegister *
xaccLedgerDisplayGetSR (xaccLedgerDisplay *ld)
{
  if (!ld)
    return NULL;

  return ld->reg;
}

Query *
xaccLedgerDisplayGetQuery (xaccLedgerDisplay *ld)
{
  if (!ld)
    return NULL;

  return ld->query;
}

static gboolean
find_by_leader (gpointer find_data, gpointer user_data)
{
  Account *account = find_data;
  xaccLedgerDisplay *ld = user_data;

  if (!account || !ld)
    return FALSE;

  return (account == xaccLedgerDisplayLeader (ld));
}

static gboolean
find_by_account (gpointer find_data, gpointer user_data)
{
  Account *account = find_data;
  xaccLedgerDisplay *ld = user_data;

  if (!account || !ld)
    return FALSE;

  if (account == xaccLedgerDisplayLeader (ld))
    return TRUE;

  if (ld->ld_type == LD_SINGLE)
    return FALSE;

  /* Hack. */
  return TRUE;
}

static gboolean
find_by_query (gpointer find_data, gpointer user_data)
{
  Query *q = find_data;
  xaccLedgerDisplay *ld = user_data;

  if (!q || !ld)
    return FALSE;

  return ld->query == q;
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

static SplitRegisterType
get_reg_type (Account *leader, LedgerDisplayType ld_type)
{
  GNCAccountType account_type;
  SplitRegisterType reg_type;
  GList *subaccounts;
  GList *node;

  if (ld_type == LD_GL)
    return GENERAL_LEDGER;

  account_type = xaccAccountGetType (leader);

  if (ld_type == LD_SINGLE)
  {
    switch (account_type)
    {
      case BANK:
        return BANK_REGISTER;

      case CASH:
        return CASH_REGISTER;

      case ASSET:
        return ASSET_REGISTER;

      case CREDIT:
        return CREDIT_REGISTER;

      case LIABILITY:
        return LIABILITY_REGISTER;

      case STOCK:
      case MUTUAL:
        return STOCK_REGISTER;

      case INCOME:
        return INCOME_REGISTER;

      case EXPENSE:
        return EXPENSE_REGISTER;

      case EQUITY:
        return EQUITY_REGISTER;

      case CURRENCY:
        return CURRENCY_REGISTER;

      default:
        PERR ("unknown account type %d\n", account_type);
        return BANK_REGISTER;
    }
  }

  if (ld_type != LD_SUBACCOUNT)
  {
    PERR ("unknown ledger type %d\n", ld_type);
    return BANK_REGISTER;
  }

  subaccounts = xaccGroupGetSubAccounts (xaccAccountGetChildren (leader));

  switch (account_type)
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

      for (node = subaccounts; node; node = node->next)
      {
        GNCAccountType le_type;

        le_type = xaccAccountGetType (node->data);
        if ((STOCK    == le_type) ||
            (MUTUAL   == le_type) ||
            (CURRENCY == le_type))
        {
          reg_type = PORTFOLIO_LEDGER;
          break;
        }
      }
      break;

    case STOCK:
    case MUTUAL:
    case CURRENCY:
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
      PERR ("unknown account type:%d", account_type);
      reg_type = GENERAL_LEDGER;
      break;
  }

  g_list_free (subaccounts);

  return reg_type;
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

  reg_type = get_reg_type (account, LD_SINGLE);

  return xaccLedgerDisplayInternal (account, NULL, LD_SINGLE, reg_type,
                                    gnc_get_default_register_style ());
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

  reg_type = get_reg_type (account, LD_SUBACCOUNT);

  ld = xaccLedgerDisplayInternal (account, NULL, LD_SUBACCOUNT,
                                  reg_type, REG_STYLE_JOURNAL);

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

  guid = g_new (GUID, 1);

  *guid = *xaccGUIDNULL ();

  return guid;
}

static void
xaccGUIDFree (gpointer _guid)
{
  GUID *guid = _guid;

  if (guid == NULL)
    return;

  *guid = *xaccGUIDNULL ();

  g_free (guid);
}

static void
xaccGUIDCopy (gpointer _to, gconstpointer _from)
{
  GUID *to = _to;
  const GUID *from = _from;

  g_return_if_fail (to != NULL);

  if (from == NULL)
    *to = *xaccGUIDNULL ();
  else
    *to = *from;
}

#if 0
static void
refresh_handler (GHashTable *changes, gpointer user_data)
{
  xaccLedgerDisplay *ld = user_data;
  const EventInfo *info;

  if (ld->type > NUM_SINGLE_REGISTER_TYPES)
  {
    Account *leader = xaccLedgerDisplayLeader (ld);
    if (!account)
    {
      gnc_close_gui_component (ld->component_id);
      return;
    }

  if (changes)
  {
    info = gnc_gui_get_entity_events (changes, &recnData->account);
    if (info && (info->event_mask & GNC_EVENT_DESTROY))
    {
      gnc_close_gui_component_by_data (WINDOW_RECONCILE_CM_CLASS, recnData);
      return;
    }
  }

  recnRefresh (recnData);
}
#endif

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

  g_free (ld);
}

static void
make_ledger_query (xaccLedgerDisplay *ld,
                   gboolean show_all,
                   SplitRegisterType type)
{
  Account *leader;
  GList *accounts;

  if (!ld)
    return;

  switch (ld->ld_type)
  {
    case LD_SINGLE:
    case LD_SUBACCOUNT:
      break;

    case LD_GL:
      return;

    default:
      PERR ("unknown ledger type: %d", ld->ld_type);
      return;
  }

  xaccFreeQuery (ld->query);
  ld->query = xaccMallocQuery ();

  /* This is a bit of a hack. The number of splits should be
   * configurable, or maybe we should go back a time range instead
   * of picking a number, or maybe we should be able to exclude
   * based on reconciled status. Anyway, this works for now. */
  if (!show_all && (type != SEARCH_LEDGER))
    xaccQuerySetMaxSplits (ld->query, 30);

  xaccQuerySetGroup (ld->query, gncGetCurrentGroup());

  leader = xaccLedgerDisplayLeader (ld);

  if (ld->ld_type == LD_SUBACCOUNT)
    accounts = xaccGroupGetSubAccounts (xaccAccountGetChildren (leader));
  else
    accounts = NULL;

  accounts = g_list_prepend (accounts, leader);

  xaccQueryAddAccountMatch (ld->query, accounts,
                            ACCT_MATCH_ANY, QUERY_OR);

  g_list_free (accounts);
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
  return xaccLedgerDisplayInternal (NULL, query, LD_GL, type, style);
}

static xaccLedgerDisplay *
xaccLedgerDisplayInternal (Account *lead_account, Query *q,
                           LedgerDisplayType ld_type,
                           SplitRegisterType reg_type,
                           SplitRegisterStyle style)
{
  xaccLedgerDisplay *ld;
  gboolean show_all;
  const char *class;

  switch (ld_type)
  {
    case LD_SINGLE:
      class = REGISTER_SINGLE_CM_CLASS;

      if (reg_type >= NUM_SINGLE_REGISTER_TYPES)
      {
        PERR ("single-account register with wrong split register type");
        return NULL;
      }

      if (!lead_account)
      {
        PERR ("single-account register with no account specified");
        return NULL;
      }

      if (q)
      {
        PWARN ("single-account register with external query");
        q = NULL;
      }

      ld = gnc_find_first_gui_component (class, find_by_leader, lead_account);
      if (ld)
        return ld;

      break;

    case LD_SUBACCOUNT:
      class = REGISTER_SUBACCOUNT_CM_CLASS;

      if (!lead_account)
      {
        PERR ("sub-account register with no lead account");
        return NULL;
      }

      if (q)
      {
        PWARN ("account register with external query");
        q = NULL;
      }

      ld = gnc_find_first_gui_component (class, find_by_leader, lead_account);
      if (ld)
        return ld;

      break;

    case LD_GL:
      class = REGISTER_GL_CM_CLASS;

      if (!q)
      {
        PWARN ("general ledger with no query");
      }

      break;

    default:
      PERR ("bad ledger type: %d", ld_type);
      return NULL;
  }

  ld = g_new (xaccLedgerDisplay, 1);

  ld->leader = *xaccAccountGetGUID (lead_account);
  ld->query = NULL;
  ld->ld_type = ld_type;
  ld->destroy = NULL;
  ld->get_parent = NULL;
  ld->set_help = NULL;
  ld->user_data = NULL;

  show_all = gnc_lookup_boolean_option ("Register",
                                        "Show All Transactions",
                                        TRUE);

  /* set up the query filter */
  if (q)
    ld->query = xaccQueryCopy (q);
  else
    make_ledger_query (ld, show_all, reg_type);

  ld->component_id = gnc_register_gui_component (class, NULL,
                                                 close_handler, ld);

  /******************************************************************\
   * The main register window itself                                *
  \******************************************************************/

  /* xaccMallocSplitRegister will malloc & initialize the register,
   * but will not do the gui init */
  ld->reg = xaccMallocSplitRegister (reg_type, style, FALSE,
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

  xaccLedgerDisplayRefresh (ld);

  return ld;
}

void
xaccLedgerDisplaySetQuery (xaccLedgerDisplay *ledger_display, Query *q)
{
  if (!ledger_display || !q)
    return;

  g_return_if_fail (ledger_display->ld_type == LD_GL);

  xaccFreeQuery (ledger_display->query);
  ledger_display->query = xaccQueryCopy (q);
}

xaccLedgerDisplay *
xaccFindGeneralLedgerByQuery (Query *q)
{
  if (!q)
    return NULL;

  return gnc_find_first_gui_component (REGISTER_GL_CM_CLASS, find_by_query, q);
}

/********************************************************************\
 * refresh only the indicated register window                       *
\********************************************************************/

void 
xaccLedgerDisplayRefresh (xaccLedgerDisplay *ld)
{
  if (!ld)
    return;

  /* The leader account is used by the register gui to
   * assign a default source account for a "blank split"
   * that is attached to the bottom of the register.
   * The "blank split" is what the user edits to create 
   * new splits and get them into the system. */
  xaccSRLoadRegister (ld->reg,
                      xaccQueryGetSplits (ld->query),
                      xaccLedgerDisplayLeader (ld));
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
