/********************************************************************\
 * gnc-ledger-display.c -- utilities for dealing with multiple      *
 *                         register/ledger windows in GnuCash       *
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

#include <time.h>

#include "Account.h"
#include "Group.h"
#include "Query.h"
#include "Transaction.h"
#include "FileDialog.h"
#include "date.h"
#include "global-options.h"
#include "gnc-component-manager.h"
#include "gnc-engine-util.h"
#include "gnc-ledger-display.h"
#include "split-register-control.h"
#include "split-register-model.h"


#define REGISTER_SINGLE_CM_CLASS     "register-single"
#define REGISTER_SUBACCOUNT_CM_CLASS "register-subaccount"
#define REGISTER_GL_CM_CLASS         "register-gl"
#define REGISTER_TEMPLATE_CM_CLASS   "register-template"


struct _xaccLedgerDisplay
{
  GUID leader;

  Query *query;

  LedgerDisplayType ld_type;

  SplitRegister *reg;

  gboolean loading;

  LedgerDisplayDestroy destroy;
  LedgerDisplayGetParent get_parent;

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
                           SplitRegisterStyle style,
                           gboolean is_template);
static void xaccLedgerDisplayRefreshInternal (xaccLedgerDisplay *ld,
                                              GList *splits);


/** Implementations *************************************************/

Account *
xaccLedgerDisplayLeader (xaccLedgerDisplay *ld)
{
  if (!ld)
    return NULL;

  return xaccAccountLookup (&ld->leader);
}

LedgerDisplayType
xaccLedgerDisplayType (xaccLedgerDisplay *ld)
{
  if (!ld)
    return -1;

  return ld->ld_type;
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
                              LedgerDisplayGetParent get_parent)
{
  if (!ld)
    return;

  ld->destroy = destroy;
  ld->get_parent = get_parent;
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

static gboolean
find_by_reg (gpointer find_data, gpointer user_data)
{
  SplitRegister *reg = find_data;
  xaccLedgerDisplay *ld = user_data;

  if (!reg || !ld)
    return FALSE;

  return ld->reg == reg;
}

static SplitRegisterStyle
gnc_get_default_register_style (void)
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
                                    gnc_get_default_register_style (), FALSE );
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
  SplitRegisterType reg_type;

  reg_type = get_reg_type (account, LD_SUBACCOUNT);

  return xaccLedgerDisplayInternal (account, NULL, LD_SUBACCOUNT,
                                    reg_type, REG_STYLE_JOURNAL, FALSE );
}



/********************************************************************\
 * xaccLedgerDisplayGL                                              *
 *   opens up a general ledger window                               *
 *                                                                  *
 * Args:   none                                                     *
 * Return: the register window instance                             *
\********************************************************************/

xaccLedgerDisplay *
xaccLedgerDisplayGL (void)
{
  Query *query;
  time_t start;
  struct tm *tm;

  query = xaccMallocQuery ();

  xaccQuerySetGroup (query, gncGetCurrentGroup());

  xaccQueryAddBalanceMatch (query,
                            BALANCE_BALANCED | BALANCE_UNBALANCED,
                            QUERY_AND);

  start = time (NULL);

  tm = localtime (&start);

  tm->tm_mon--;
  tm->tm_hour = 0;
  tm->tm_min = 0;
  tm->tm_sec = 0;
  tm->tm_isdst = -1;

  start = mktime (tm);

  xaccQueryAddDateMatchTT (query, 
                           TRUE, start, 
                           FALSE, 0, 
                           QUERY_AND);

  return xaccLedgerDisplayInternal (NULL, query, LD_GL,
                                    GENERAL_LEDGER,
                                    REG_STYLE_JOURNAL, FALSE );
}

/**
 * id is some identifier that can be:
 * . used in a query to look for the transaction which belong to this
 *   template ledger
 * . set in a specific key value for new transactions which belong to
 *   this template ledger.
 **/
xaccLedgerDisplay *
xaccLedgerDisplayTemplateGL (char *id)
{
  GNCBook *book;
  Query *q;
  time_t start;
  struct tm *tm;
  xaccLedgerDisplay *ld;
  SplitRegister *sr;
  AccountGroup *ag;
  Account *acct;

  q = xaccMallocQuery ();

  ag = gnc_book_get_template_group (gncGetCurrentBook());
  acct = xaccGetAccountFromName (ag, id);
  if (!acct)
  {
    /* FIXME */
    printf( "can't get template account for id \"%s\"\n", id );
  }

  xaccQueryAddSingleAccountMatch (q, acct, QUERY_AND);
  book = gncGetCurrentBook ();
  xaccQuerySetGroup (q, gnc_book_get_template_group(book));

  ld = xaccLedgerDisplayInternal (NULL, q, LD_GL,
                                  GENERAL_LEDGER,
                                  REG_STYLE_JOURNAL,
                                  TRUE); /* template mode?  TRUE. */

  sr = xaccLedgerDisplayGetSR (ld);

  gnc_split_register_set_template_account (sr, acct);

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
ledger_set_watches (xaccLedgerDisplay *ld, GList *splits)
{
  GList *node;

  gnc_gui_component_clear_watches (ld->component_id);

  gnc_gui_component_watch_entity_type (ld->component_id,
                                       GNC_ID_ACCOUNT,
                                       GNC_EVENT_MODIFY | GNC_EVENT_DESTROY);

  for (node = splits; node; node = node->next)
  {
    Split *split = node->data;
    Transaction *trans = xaccSplitGetParent (split);

    gnc_gui_component_watch_entity (ld->component_id,
                                    xaccTransGetGUID (trans),
                                    GNC_EVENT_MODIFY);
  }
}

static void
refresh_handler (GHashTable *changes, gpointer user_data)
{
  xaccLedgerDisplay *ld = user_data;
  const EventInfo *info;
  gboolean has_leader;
  GList *splits;

  if (ld->loading)
    return;

  has_leader = (ld->ld_type == LD_SINGLE || ld->ld_type == LD_SUBACCOUNT);

  if (has_leader)
  {
    Account *leader = xaccLedgerDisplayLeader (ld);
    if (!leader)
    {
      gnc_close_gui_component (ld->component_id);
      return;
    }
  }

  if (changes && has_leader)
  {
    info = gnc_gui_get_entity_events (changes, &ld->leader);
    if (info && (info->event_mask & GNC_EVENT_DESTROY))
    {
      gnc_close_gui_component (ld->component_id);
      return;
    }
  }

  splits = xaccQueryGetSplits (ld->query);

  ledger_set_watches (ld, splits);

  xaccLedgerDisplayRefreshInternal (ld, splits);
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

  gnc_split_register_destroy (ld->reg);
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
    case LD_TEMPLATE:
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
  return xaccLedgerDisplayInternal (NULL, query, LD_GL, type, style, FALSE);
}

static xaccLedgerDisplay *
xaccLedgerDisplayInternal (Account *lead_account, Query *q,
                           LedgerDisplayType ld_type,
                           SplitRegisterType reg_type,
                           SplitRegisterStyle style,
                           gboolean is_template )
{
  xaccLedgerDisplay *ld;
  gboolean show_all;
  const char *class;
  GList *splits;

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

  case LD_TEMPLATE:
    class = REGISTER_TEMPLATE_CM_CLASS;
    /* FIXME: sanity checks?
       Check for kvp-frame data? */
    break;
    
    default:
      PERR ("bad ledger type: %d", ld_type);
      return NULL;

  }

  ld = g_new (xaccLedgerDisplay, 1);

  ld->leader = *xaccAccountGetGUID (lead_account);
  ld->query = NULL;
  ld->ld_type = ld_type;
  ld->loading = FALSE;
  ld->destroy = NULL;
  ld->get_parent = NULL;
  ld->user_data = NULL;

  show_all = gnc_lookup_boolean_option ("Register",
                                        "Show All Transactions",
                                        TRUE);

  /* set up the query filter */
  if (q)
    ld->query = xaccQueryCopy (q);
  else
    make_ledger_query (ld, show_all, reg_type);

  ld->component_id = gnc_register_gui_component (class,
                                                 refresh_handler,
                                                 close_handler, ld);

  /******************************************************************\
   * The main register window itself                                *
  \******************************************************************/

  ld->reg = gnc_split_register_new (reg_type, style, FALSE, is_template);

  gnc_split_register_set_data (ld->reg, ld, xaccLedgerDisplayParent);

  splits = xaccQueryGetSplits (ld->query);

  ledger_set_watches (ld, splits);

  xaccLedgerDisplayRefreshInternal (ld, splits);

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

static void
xaccLedgerDisplayRefreshInternal (xaccLedgerDisplay *ld, GList *splits)
{
  if (!ld || ld->loading)
    return;

  if (!gnc_split_register_full_refresh_ok (ld->reg))
  {
    gnc_split_register_load_xfer_cells (ld->reg, xaccLedgerDisplayLeader (ld));
    return;
  }

  ld->loading = TRUE;

  gnc_split_register_load (ld->reg, splits, xaccLedgerDisplayLeader (ld));

  ld->loading = FALSE;
}

void
xaccLedgerDisplayRefresh (xaccLedgerDisplay *ld)
{
  if (!ld || ld->loading)
    return;

  xaccLedgerDisplayRefreshInternal (ld, xaccQueryGetSplits (ld->query));
}

void
xaccLedgerDisplayRefreshByReg (SplitRegister *reg)
{
  xaccLedgerDisplay *ld;

  if (!reg)
    return;

  ld = gnc_find_first_gui_component (REGISTER_SINGLE_CM_CLASS,
                                     find_by_reg, reg);
  if (ld)
  {
    xaccLedgerDisplayRefresh (ld);
    return;
  }

  ld = gnc_find_first_gui_component (REGISTER_SUBACCOUNT_CM_CLASS,
                                     find_by_reg, reg);
  if (ld)
  {
    xaccLedgerDisplayRefresh (ld);
    return;
  }

  ld = gnc_find_first_gui_component (REGISTER_GL_CM_CLASS,
                                     find_by_reg, reg);
  if (ld)
  {
    xaccLedgerDisplayRefresh (ld);
    return;
  }

  ld = gnc_find_first_gui_component (REGISTER_TEMPLATE_CM_CLASS,
                                     find_by_reg, reg );
  if (ld)
    {
      xaccLedgerDisplayRefresh (ld);
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

  g_list_free (list);
}

void
xaccDestroyLedgerDisplay (Account *account)
{
  if (!account)
    return;

  xaccDestroyLedgerDisplayClass (account, REGISTER_SINGLE_CM_CLASS);
  xaccDestroyLedgerDisplayClass (account, REGISTER_SUBACCOUNT_CM_CLASS);
  xaccDestroyLedgerDisplayClass (account, REGISTER_GL_CM_CLASS);
  /* no TEMPLATE_CM_CLASS, because it doesn't correspond to any account
   * FIXME: but there probably should be an analagous method */
}

void
xaccLedgerDisplayClose (xaccLedgerDisplay *ld)
{
  if (!ld)
    return;

  gnc_close_gui_component (ld->component_id);
}
