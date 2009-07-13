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
#include "Query.h"
#include "QueryCore.h"
#include "QueryNew.h"
#include "SX-book.h"
#include "Transaction.h"
#include "gnc-component-manager.h"
#include "gnc-book.h"
#include "gnc-date.h"
#include "gnc-engine.h"
#include "gnc-event.h"
#include "gnc-gconf-utils.h"
#include "gnc-ledger-display.h"
#include "gnc-ui-util.h"
#include "split-register-control.h"
#include "split-register-model.h"


#define REGISTER_SINGLE_CM_CLASS     "register-single"
#define REGISTER_SUBACCOUNT_CM_CLASS "register-subaccount"
#define REGISTER_GL_CM_CLASS         "register-gl"
#define REGISTER_TEMPLATE_CM_CLASS   "register-template"


struct gnc_ledger_display
{
  GUID leader;

  Query *query;

  GNCLedgerDisplayType ld_type;

  SplitRegister *reg;

  gboolean loading;
  gboolean use_double_line_default;

  GNCLedgerDisplayDestroy destroy;
  GNCLedgerDisplayGetParent get_parent;

  gpointer user_data;

  gint component_id;
};


/** GLOBALS *********************************************************/
static QofLogModule log_module = GNC_MOD_LEDGER;


/** Declarations ****************************************************/
static GNCLedgerDisplay *
gnc_ledger_display_internal (Account *lead_account, Query *q,
                             GNCLedgerDisplayType ld_type,
                             SplitRegisterType reg_type,
                             SplitRegisterStyle style,
                             gboolean use_double_line,
                             gboolean is_template);
static void gnc_ledger_display_refresh_internal (GNCLedgerDisplay *ld,
                                                 GList *splits);


/** Implementations *************************************************/

Account *
gnc_ledger_display_leader (GNCLedgerDisplay *ld)
{
  if (!ld)
    return NULL;

  return xaccAccountLookup (&ld->leader, gnc_get_current_book ());
}

GNCLedgerDisplayType
gnc_ledger_display_type (GNCLedgerDisplay *ld)
{
  if (!ld)
    return -1;

  return ld->ld_type;
}

void
gnc_ledger_display_set_user_data (GNCLedgerDisplay *ld, gpointer user_data)
{
  if (!ld)
    return;

  ld->user_data = user_data;
}

gpointer
gnc_ledger_display_get_user_data (GNCLedgerDisplay *ld)
{
  if (!ld)
    return NULL;

  return ld->user_data;
}

void
gnc_ledger_display_set_handlers (GNCLedgerDisplay *ld,
                                 GNCLedgerDisplayDestroy destroy,
                                 GNCLedgerDisplayGetParent get_parent)
{
  if (!ld)
    return;

  ld->destroy = destroy;
  ld->get_parent = get_parent;
}

SplitRegister *
gnc_ledger_display_get_split_register (GNCLedgerDisplay *ld)
{
  if (!ld)
    return NULL;

  return ld->reg;
}

Query *
gnc_ledger_display_get_query (GNCLedgerDisplay *ld)
{
  if (!ld)
    return NULL;

  return ld->query;
}

static gboolean
find_by_leader (gpointer find_data, gpointer user_data)
{
  Account *account = find_data;
  GNCLedgerDisplay *ld = user_data;

  if (!account || !ld)
    return FALSE;

  return (account == gnc_ledger_display_leader (ld));
}

static gboolean
find_by_query (gpointer find_data, gpointer user_data)
{
  Query *q = find_data;
  GNCLedgerDisplay *ld = user_data;

  if (!q || !ld)
    return FALSE;

  return ld->query == q;
}

static gboolean
find_by_reg (gpointer find_data, gpointer user_data)
{
  SplitRegister *reg = find_data;
  GNCLedgerDisplay *ld = user_data;

  if (!reg || !ld)
    return FALSE;

  return ld->reg == reg;
}

static SplitRegisterStyle
gnc_get_default_register_style (GNCAccountType type)
{
  SplitRegisterStyle new_style = REG_STYLE_LEDGER;
  gchar *style_string;

  switch (type) {
#if 0
  case ACCT_TYPE_PAYABLE:
  case ACCT_TYPE_RECEIVABLE:
    new_style = REG_STYLE_LEDGER;
    break;
#endif

  default:
    style_string = gnc_gconf_get_string(GCONF_GENERAL_REGISTER,
					"default_style", NULL);
    if (safe_strcmp(style_string, "journal") == 0)
      new_style = REG_STYLE_JOURNAL;
    else if (safe_strcmp(style_string, "auto_ledger") == 0)
      new_style = REG_STYLE_AUTO_LEDGER;
    else
      new_style = REG_STYLE_LEDGER;

    if (style_string != NULL)
      g_free(style_string);

    break;
  }

  return new_style;
}

static gpointer
look_for_portfolio_cb (Account *account, gpointer data)
{
    return xaccAccountIsPriced(account) ? (gpointer) PORTFOLIO_LEDGER : NULL;
}

static SplitRegisterType
gnc_get_reg_type (Account *leader, GNCLedgerDisplayType ld_type)
{
  GNCAccountType account_type;
  SplitRegisterType reg_type;

  if (ld_type == LD_GL)
    return GENERAL_LEDGER;

  account_type = xaccAccountGetType (leader);

  if (ld_type == LD_SINGLE)
  {
    switch (account_type)
    {
      case ACCT_TYPE_BANK:
        return BANK_REGISTER;

      case ACCT_TYPE_CASH:
        return CASH_REGISTER;

      case ACCT_TYPE_ASSET:
        return ASSET_REGISTER;

      case ACCT_TYPE_CREDIT:
        return CREDIT_REGISTER;

      case ACCT_TYPE_LIABILITY:
        return LIABILITY_REGISTER;

      case ACCT_TYPE_PAYABLE:
        return PAYABLE_REGISTER;

      case ACCT_TYPE_RECEIVABLE:
        return RECEIVABLE_REGISTER;

      case ACCT_TYPE_STOCK:
      case ACCT_TYPE_MUTUAL:
        return STOCK_REGISTER;

      case ACCT_TYPE_INCOME:
        return INCOME_REGISTER;

      case ACCT_TYPE_EXPENSE:
        return EXPENSE_REGISTER;

      case ACCT_TYPE_EQUITY:
        return EQUITY_REGISTER;

      case ACCT_TYPE_CURRENCY:
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

  switch (account_type)
  {
    case ACCT_TYPE_BANK:
    case ACCT_TYPE_CASH:
    case ACCT_TYPE_ASSET:
    case ACCT_TYPE_CREDIT:
    case ACCT_TYPE_LIABILITY:
    case ACCT_TYPE_RECEIVABLE:
    case ACCT_TYPE_PAYABLE:
    {
      /* If any of the sub-accounts have ACCT_TYPE_STOCK or
       * ACCT_TYPE_MUTUAL types, then we must use the PORTFOLIO_LEDGER
       * ledger. Otherwise, a plain old GENERAL_LEDGER will do. */
      gpointer ret;
      reg_type = GENERAL_LEDGER;

      ret = gnc_account_foreach_descendant_until(leader, look_for_portfolio_cb, NULL);
      if (ret) reg_type = PORTFOLIO_LEDGER;
      break;
    }

    case ACCT_TYPE_STOCK:
    case ACCT_TYPE_MUTUAL:
    case ACCT_TYPE_CURRENCY:
      reg_type = PORTFOLIO_LEDGER;
      break;

    case ACCT_TYPE_INCOME:
    case ACCT_TYPE_EXPENSE:
      reg_type = INCOME_LEDGER;
      break;

    case ACCT_TYPE_EQUITY:
      reg_type = GENERAL_LEDGER;
      break;

    default:
      PERR ("unknown account type:%d", account_type);
      reg_type = GENERAL_LEDGER;
      break;
  }

  return reg_type;
}

/* Returns a boolean of whether this display should be single or double lined
 * mode by default */
gboolean
gnc_ledger_display_default_double_line (GNCLedgerDisplay *gld)
{
  return (gld->use_double_line_default ||
          gnc_gconf_get_bool(GCONF_GENERAL_REGISTER, "double_line_mode", NULL));
}

/* Opens up a register window to display a single account */
GNCLedgerDisplay *
gnc_ledger_display_simple (Account *account)
{
  SplitRegisterType reg_type;
  GNCAccountType acc_type = xaccAccountGetType (account);
  gboolean use_double_line;
  GNCLedgerDisplay *ld;

  ENTER("account=%p", account);

  switch (acc_type) {
  case ACCT_TYPE_PAYABLE:
  case ACCT_TYPE_RECEIVABLE:
    use_double_line = TRUE;
    break;
  default:
    use_double_line = FALSE;
    break;
  }

  reg_type = gnc_get_reg_type (account, LD_SINGLE);

  ld = gnc_ledger_display_internal (account, NULL, LD_SINGLE, reg_type,
                                    gnc_get_default_register_style(acc_type),
                                    use_double_line, FALSE);
  LEAVE("%p", ld);
  return ld;
}

/* Opens up a register window to display an account, and all of its
 *   children, in the same window */
GNCLedgerDisplay *
gnc_ledger_display_subaccounts (Account *account)
{
  SplitRegisterType reg_type;
  GNCLedgerDisplay *ld;

  ENTER("account=%p", account);

  reg_type = gnc_get_reg_type (account, LD_SUBACCOUNT);

  ld = gnc_ledger_display_internal (account, NULL, LD_SUBACCOUNT,
                                    reg_type, REG_STYLE_JOURNAL, FALSE,
                                    FALSE);
  LEAVE("%p", ld);
  return ld;
}

/* Opens up a general ledger window. */
GNCLedgerDisplay *
gnc_ledger_display_gl (void)
{
  Query *query;
  time_t start;
  struct tm tm;
  GNCLedgerDisplay *ld;

  ENTER(" ");

  query = xaccMallocQuery ();

  xaccQuerySetBook (query, gnc_get_current_book());

  /* In lieu of not "mis-using" some portion of the infrastructure by writing
   * a bunch of new code, we just filter out the accounts of the template
   * transactions.  While these are in a seperate Account trees just for this
   * reason, the query engine makes no distinction between Account trees.
   * See Gnome Bug 86302.
   *         -- jsled */
  {
    Account *tRoot;
    GList *al;
    
    tRoot = gnc_book_get_template_root( gnc_get_current_book() );
    al = gnc_account_get_descendants( tRoot );
    xaccQueryAddAccountMatch( query, al, GUID_MATCH_NONE, QUERY_AND );
    g_list_free (al);
    al = NULL;
    tRoot = NULL;
  }

  gnc_tm_get_today_start(&tm);
  tm.tm_mon--; /* Default the register to the last month's worth of transactions. */
  start = mktime (&tm);
  xaccQueryAddDateMatchTT (query, 
                           TRUE, start, 
                           FALSE, 0, 
                           QUERY_AND);

  ld = gnc_ledger_display_internal (NULL, query, LD_GL, GENERAL_LEDGER,
                                    REG_STYLE_JOURNAL, FALSE, FALSE);
  LEAVE("%p", ld);
  return ld;
}

/**
 * @param id: The string version of the GUID of the context of template
 * transaction being edited in this template GL.  As used by scheduled
 * transactions, this is the GUID of the SX itself which is magically the
 * *name* of the (template) account which contains the transactions for this
 * scheduled transaction.  That's right.  The stringified GUID of the SX is
 * the name of the SX'es template account.
 **/
GNCLedgerDisplay *
gnc_ledger_display_template_gl (char *id)
{
  QofBook *book;
  Query *q;
  GNCLedgerDisplay *ld;
  SplitRegister *sr;
  Account *root, *acct;
  gboolean isTemplateModeTrue;

  ENTER("id=%s", id? id : "(null)");

  acct = NULL;
  isTemplateModeTrue = TRUE;

  q = xaccMallocQuery ();

  book = gnc_get_current_book ();
  xaccQuerySetBook (q, book);

  if ( id != NULL ) {
    root = gnc_book_get_template_root (book);
    acct = gnc_account_lookup_by_name(root, id);
    g_assert( acct );
    xaccQueryAddSingleAccountMatch (q, acct, QUERY_AND);
  }

  ld = gnc_ledger_display_internal (NULL, q, LD_GL,
                                    SEARCH_LEDGER,
                                    REG_STYLE_JOURNAL,
                                    FALSE,
                                    isTemplateModeTrue);

  sr = gnc_ledger_display_get_split_register (ld);
  if ( acct ) {
    gnc_split_register_set_template_account (sr, acct);
  }

  LEAVE("%p", ld);
  return ld;
}

gncUIWidget
gnc_ledger_display_get_parent( GNCLedgerDisplay *ld )
{
  if ( ld == NULL )
    return NULL;

  if ( ld->get_parent == NULL )
    return NULL;

  return ld->get_parent( ld );
}

static gncUIWidget
gnc_ledger_display_parent (void *user_data)
{
  GNCLedgerDisplay *ld = user_data;
  return gnc_ledger_display_get_parent( ld );
}

static void
gnc_ledger_display_set_watches (GNCLedgerDisplay *ld, GList *splits)
{
  GList *node;

  gnc_gui_component_clear_watches (ld->component_id);

  gnc_gui_component_watch_entity_type (ld->component_id,
                                       GNC_ID_ACCOUNT,
                                       QOF_EVENT_MODIFY | QOF_EVENT_DESTROY
				       | GNC_EVENT_ITEM_CHANGED);

  for (node = splits; node; node = node->next)
  {
    Split *split = node->data;
    Transaction *trans = xaccSplitGetParent (split);

    gnc_gui_component_watch_entity (ld->component_id,
                                    xaccTransGetGUID (trans),
                                    QOF_EVENT_MODIFY);
  }
}

static void
refresh_handler (GHashTable *changes, gpointer user_data)
{
  GNCLedgerDisplay *ld = user_data;
  const EventInfo *info;
  gboolean has_leader;
  GList *splits;

  ENTER("changes=%p, user_data=%p", changes, user_data);

  if (ld->loading)
  {
    LEAVE("already loading");
    return;
  }

  has_leader = (ld->ld_type == LD_SINGLE || ld->ld_type == LD_SUBACCOUNT);

  if (has_leader)
  {
    Account *leader = gnc_ledger_display_leader (ld);
    if (!leader)
    {
      gnc_close_gui_component (ld->component_id);
      LEAVE("no leader");
      return;
    }
  }

  if (changes && has_leader)
  {
    info = gnc_gui_get_entity_events (changes, &ld->leader);
    if (info && (info->event_mask & QOF_EVENT_DESTROY))
    {
      gnc_close_gui_component (ld->component_id);
      LEAVE("destroy");
      return;
    }
  }

  /* Its not clear if we should re-run the query, or if we should
   * just use qof_query_last_run().  Its possible that the dates
   * changed, requiring a full new query.  Similar considerations
   * needed for multi-user mode.
   */
  splits = qof_query_run (ld->query);

  gnc_ledger_display_set_watches (ld, splits);

  gnc_ledger_display_refresh_internal (ld, splits);
  LEAVE(" ");
}

static void
close_handler (gpointer user_data)
{
  GNCLedgerDisplay *ld = user_data;

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
gnc_ledger_display_make_query (GNCLedgerDisplay *ld,
                               gint limit,
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
  if ((limit != 0) && (type != SEARCH_LEDGER))
    xaccQuerySetMaxSplits (ld->query, limit);

  xaccQuerySetBook (ld->query, gnc_get_current_book());

  leader = gnc_ledger_display_leader (ld);

  if (ld->ld_type == LD_SUBACCOUNT)
    accounts = gnc_account_get_descendants (leader);
  else
    accounts = NULL;

  accounts = g_list_prepend (accounts, leader);

  xaccQueryAddAccountMatch (ld->query, accounts,
                            GUID_MATCH_ANY, QUERY_AND);

  g_list_free (accounts);
}

/* Opens up a ledger window for an arbitrary query. */
GNCLedgerDisplay *
gnc_ledger_display_query (Query *query, SplitRegisterType type,
                          SplitRegisterStyle style)
{
  GNCLedgerDisplay *ld;

  ENTER("query=%p", query);

  ld = gnc_ledger_display_internal (NULL, query, LD_GL, type, style,
                                    FALSE, FALSE);
  LEAVE("%p", ld);
  return ld;
}

static GNCLedgerDisplay *
gnc_ledger_display_internal (Account *lead_account, Query *q,
                             GNCLedgerDisplayType ld_type,
                             SplitRegisterType reg_type,
                             SplitRegisterStyle style,
                             gboolean use_double_line,
                             gboolean is_template )
{
  GNCLedgerDisplay *ld;
  gint limit;
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

    default:
      PERR ("bad ledger type: %d", ld_type);
      return NULL;

  }

  ld = g_new (GNCLedgerDisplay, 1);

  ld->leader = *xaccAccountGetGUID (lead_account);
  ld->query = NULL;
  ld->ld_type = ld_type;
  ld->loading = FALSE;
  ld->destroy = NULL;
  ld->get_parent = NULL;
  ld->user_data = NULL;

  limit = gnc_gconf_get_float(GCONF_GENERAL_REGISTER, "max_transactions", NULL);

  /* set up the query filter */
  if (q)
    ld->query = xaccQueryCopy (q);
  else
    gnc_ledger_display_make_query (ld, limit, reg_type);

  ld->component_id = gnc_register_gui_component (class,
                                                 refresh_handler,
                                                 close_handler, ld);

  /******************************************************************\
   * The main register window itself                                *
  \******************************************************************/

  ld->use_double_line_default = use_double_line;
  ld->reg = gnc_split_register_new (reg_type, style, use_double_line,
                                    is_template);

  gnc_split_register_set_data (ld->reg, ld, gnc_ledger_display_parent);

  splits = qof_query_run (ld->query);

  gnc_ledger_display_set_watches (ld, splits);

  gnc_ledger_display_refresh_internal (ld, splits);

  return ld;
}

void
gnc_ledger_display_set_query (GNCLedgerDisplay *ledger_display, Query *q)
{
  if (!ledger_display || !q)
    return;

  g_return_if_fail (ledger_display->ld_type == LD_GL);

  xaccFreeQuery (ledger_display->query);
  ledger_display->query = xaccQueryCopy (q);
}

GNCLedgerDisplay *
gnc_ledger_display_find_by_query (Query *q)
{
  if (!q)
    return NULL;

  return gnc_find_first_gui_component (REGISTER_GL_CM_CLASS, find_by_query, q);
}

/********************************************************************\
 * refresh only the indicated register window                       *
\********************************************************************/

static void
gnc_ledger_display_refresh_internal (GNCLedgerDisplay *ld, GList *splits)
{
  if (!ld || ld->loading)
    return;

  if (!gnc_split_register_full_refresh_ok (ld->reg))
    return;

  ld->loading = TRUE;

  gnc_split_register_load (ld->reg, splits,
                           gnc_ledger_display_leader (ld));

  ld->loading = FALSE;
}

void
gnc_ledger_display_refresh (GNCLedgerDisplay *ld)
{
  ENTER("ld=%p", ld);

  if (!ld)
  {
    LEAVE("no display");
    return;
  }

  if (ld->loading)
  {
    LEAVE("already loading");
    return;
  }

  gnc_ledger_display_refresh_internal (ld, xaccQueryGetSplits (ld->query));
  LEAVE(" ");
}

void
gnc_ledger_display_refresh_by_split_register (SplitRegister *reg)
{
  GNCLedgerDisplay *ld;

  if (!reg)
    return;

  ld = gnc_find_first_gui_component (REGISTER_SINGLE_CM_CLASS,
                                     find_by_reg, reg);
  if (ld)
  {
    gnc_ledger_display_refresh (ld);
    return;
  }

  ld = gnc_find_first_gui_component (REGISTER_SUBACCOUNT_CM_CLASS,
                                     find_by_reg, reg);
  if (ld)
  {
    gnc_ledger_display_refresh (ld);
    return;
  }

  ld = gnc_find_first_gui_component (REGISTER_GL_CM_CLASS,
                                     find_by_reg, reg);
  if (ld)
  {
    gnc_ledger_display_refresh (ld);
    return;
  }

  ld = gnc_find_first_gui_component (REGISTER_TEMPLATE_CM_CLASS,
                                     find_by_reg, reg );
  if (ld)
  {
    gnc_ledger_display_refresh (ld);
  }
}

void
gnc_ledger_display_close (GNCLedgerDisplay *ld)
{
  if (!ld)
    return;

  gnc_close_gui_component (ld->component_id);
}
