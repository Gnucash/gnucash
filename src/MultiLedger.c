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

#include "SplitLedger.h"
#include "LedgerUtils.h"
#include "Transaction.h"
#include "util.h"

/** STRUCTS *********************************************************/
/* The LedgerWindow struct contains data describing a single instance
 * of some displayed transactions.
 */

typedef struct _LedgerWindow LedgerWindow;

struct _LedgerWindow {
  Account *lead_acct;            /* leading. "master" account               */
  Account **displayed_accounts;  /* The list of accounts shown here         */
  short   numAcc;                /* number of accounts in list              */

  short type;                    /* register display type, usually equal to *
                                  * account type                            */

  short dirty;                   /* dirty flag, non zero if redraw needed   */
  SplitRegister *ledger;         /* main ledger window                      */
  void *gui_hook;                /* GUI-specific state                      */
};


/** GLOBALS *********************************************************/
/* These are globals because they describe the state of the entire session.
 * The is, there must be only one instance of these per GUI session.
 */

static LedgerWindow **regList = NULL;     /* single-account registers */
static LedgerWindow **ledgerList = NULL;  /* multiple-account registers */
static LedgerWindow **fullList = NULL;    /* all registers */

/** PROTOTYPES ******************************************************/
LedgerWindow * regWindowLedger( Widget parent, Account *lead, Account **acclist, int type);
void        accRefresh (Account *acc);
void        regRefresh (LedgerWindow *regData);


/********************************************************************\
 * Ledger utilities                                                 *
 * should replace with glib calls, as per g* style guides.          *
\********************************************************************/

int 
ledgerListCount (LedgerWindow **list)
{
   int n = 0;
   if (!list) return 0;
   while (list[n]) n++;
   return n;
}

/* ------------------------------------------------------ */

LedgerWindow ** 
ledgerListAdd (LedgerWindow **oldlist, LedgerWindow *addreg)
{
   LedgerWindow **newlist;
   LedgerWindow *reg;
   int n;

   if (!addreg) return oldlist;

   n = ledgerListCount (oldlist);
   newlist = (LedgerWindow **) _malloc ((n+2) * sizeof (LedgerWindow *));

   n = 0;
   if (oldlist) {
      reg = oldlist[0];
      while (reg) {
         newlist[n] = reg;
         n++;
         reg = oldlist[n];
      }
      _free (oldlist);
   }
   newlist[n] = addreg;
   newlist[n+1] = NULL;

   return newlist;
}

/* ------------------------------------------------------ */

void
ledgerListRemove (LedgerWindow **list, LedgerWindow *delreg)
{
   int n, i;

   if (!list) return;
   if (!delreg) return;

   n = 0;
   i = 0; 
   while (list[n]) {
      list[i] = list[n];
      if (delreg == list[n]) i--;
      i++;
      n++;
   }
   list[i] = NULL;
}

/* ------------------------------------------------------ */

int
ledgerIsMember (LedgerWindow *reg, Account * acc)
{
   int n; 

   if (!acc) return 0;
   if (!reg) return 0;

   if (acc == reg->lead_acct) return 1;

   if (! (reg->displayed_accounts)) return 0; 

   n = 0;
   while (reg->displayed_accounts[n]) {
      if (acc == reg->displayed_accounts[n]) return 1;
      n++;
   }
   return 0;
}

/********************************************************************\
 * regWindowSimple                                                  *
 *   opens up a register window to display a single account         *
 *                                                                  *
 * Args:   acc     - the account associated with this register      *
 * Return: regData - the register window instance                   *
\********************************************************************/

LedgerWindow *
LedgerWindowSimple (Account *acc)
  {
  LedgerWindow *retval;
  int acc_type;
  int reg_type = -1;

  acc_type = xaccAccountGetType (acc);

  /* translate between different enumerants */
  switch (acc_type) {
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
    }

  retval = regWindowLedger (acc, NULL, reg_type);
  return retval;
  }

/********************************************************************\
 * LedgerWindowAccGroup                                             *
 *   opens up a register window to display an account, and all      *
 *   of its children, in the same window                            *
 *                                                                  *
 * Args:   acc     - the account associated with this register      *
 * Return: regData - the register window instance                   *
\********************************************************************/

LedgerWindow *
LedgerWindowAccGroup (Account *acc)
  {
  LedgerWindow *retval;
  Account **list;
  int ledger_type;
  Account *le;
  int n;
  int acc_type, le_type;

  /* build a flat list from the tree */
  list = xaccGroupToList (acc);

  acc_type = xaccAccountGetType (acc);
  switch (acc_type) {
    case BANK:
    case CASH:
    case ASSET:
    case CREDIT:
    case LIABILITY:
       /* if any of the sub-accounts have STOCK or MUTUAL types,
        * then we must use the PORTFOLIO type ledger.  Otherise,
        * a plain old GEN_LEDGER will do. */
       ledger_type = GENERAL_LEDGER;

       le = list[0];
       n = 0;
       while (le) {
          le_type = xaccAccountGetType (le);
          if ((STOCK == le_type) || (MUTUAL == le_type)) {
             ledger_type = PORTFOLIO;
          }
          n++;
          le = list[n];
       }
       break;

    case STOCK:
    case MUTUAL:
       ledger_type = PORTFOLIO;
       break;
    
    case INCOME:
    case EXPENSE:
       ledger_type = INCOME_LEDGER;
       break;

    case EQUITY:
       ledger_type = GENERAL_LEDGER;
       break;

    default:
      PERR (" LedgerWindowAccGroup(): unknown account type \n");
      _free (list);
      return NULL;
  }
  retval = LedgerWindowLedger (acc, list, ledger_type);

  if (list) _free (list);

  return retval;
  }

/********************************************************************\
 * LedgerWindowLedger                                               *
 *   opens up a ledger window for a list of accounts                *
 *                                                                  *
 * Args:   lead_acc - the account associated with this register     *
 *                     (may be null)                                *
 *         acc_list - the list of accounts to display in register   *
 *                     (may be null)                                *
 * Return: regData  - the register window instance                  *
\********************************************************************/

LedgerWindow *
LedgerWindowLedger (Account *lead_acc, Account **acclist, int ledger_type)
  {
  LedgerWindow   *regData = NULL;
  int    position=0;
  char *windowname;

  /******************************************************************\
  \******************************************************************/

  /* the two macros below will search for a register windows associated
   * with the leading account.  If they exist, then they will be returned,
   * and that will be that.  If they do not exist, they will be created.
   *
   * There are two lists for lead-accounts: simple, single-account 
   * registers, which display one account only, and multiple-account
   * registers.  A leading account can have at most one of each. 
   * For a multiple-account register with a lead_acct, all accounts
   * shown in the register are sub-accounts of the lead_acct.
   *
   * A third possibility exists: a multiple-account register, with
   * no lead_acct account.  In such a case, the list of accounts being
   * displayed have no particular relationshp to each other.  There
   * can be an arbitrary number of multiple-account lead_acct-less
   * registers.
   */
  regData = NULL;
  if (lead_acc) {
     if (!acclist) {
       FETCH_FROM_LIST (LedgerWindow, regList, lead_acc, lead_acct, regData);
     } else {
       FETCH_FROM_LIST (LedgerWindow, ledgerList, lead_acc, lead_acct, regData);
     }
  }
  
  /* if regData is null, then no lead_acct account was specified */
  if (!regData) {
    regData = (LedgerWindow *) malloc (sizeof (LedgerWindow));
    regData->lead_acct = NULL;
  }

  /* count the number of accounts we are supposed to display,
   * and then, store them. */
  regData->numAcc = accListCount (acclist);
  regData->displayed_accounts = accListCopy (acclist);
  regData->type = ledger_type;

  fullList = ledgerListAdd (fullList, regData);

  /* create GUI here */
  
  regData->dirty = 1;
  regRefresh (regData);
  
  return regData;
}

/********************************************************************\
 * refresh only the indicated register window                       *
\********************************************************************/

void regRefresh (LedgerWindow *regData)
{
   /* If we don't really need the redraw, don't do it. */
   if (!(regData->dirty)) return;
   regData->dirty = 0;  /* mark clean */

   /* The lead_acct account is used by the register gui to
    * assign a default source account for a "blank split"
    * that is attached to the bottom of the register.
    * The "blank split" is what the user edits to create 
    * new splits and get them into the system.
    */
   xaccBRLoadRegister (regData->ledger, 
                     xaccAccountGetSplitList (regData->lead_acct),
                     regData->lead_acct);


  /* hack alert -- this computation of totals is incorrect 
   * for multi-account ledgers */

  /* provide some convenience data for the ture GUI window.
   * If the GUI wants to display yet other stuff, its on its own.
   */

  if( NULL != regData->balance ) {
    double prt_balance, prt_clearedBalance;
    prt_balance = xaccAccountGetBalance (regData->lead_acct);
    prt_clearedBalance = xaccAccountGetClearedBalance (regData->lead_acct);

    /* for income and expense acounts, we have to reverse
     * the meaning of balance, since, in a dual entry
     * system, income will show up as a credit to a
     * bank account, and a debit to the income account.
     * Thus, positive and negative are interchanged */
    if ((INCOME_REGISTER == regData->type) ||
        (EXPENSE_REGISTER == regData->type)) { 
      prt_balance = -prt_balance;
      prt_clearedBalance = -prt_clearedBalance;
    }
  }

  /* OK, now tell this specific GUI window to redraw itself ... */

}

/********************************************************************\
 * mark dirty *all* register windows which contain this account      * 
\********************************************************************/

static void MarkDirtyAllRegs (Account *acc)
{
   LedgerWindow *regData;
   int n;

   if (!acc) return;

   /* find all registers which contain this account */
   n = 0; regData = fullList[n];
   while (regData) {
      if (ledgerIsMember (regData, acc)) {
        regData->dirty = 1;
      }
      n++; regData = fullList[n];
   }
}

/********************************************************************\
 * refresh *all* register windows which contain this account        * 
\********************************************************************/

static void RefreshAllRegs (Account *acc)
{
   LedgerWindow *regData;
   int n;

   if (!acc) return;

   /* find all registers which contain this account */
   n = 0; regData = fullList[n];
   while (regData) {
      if (ledgerIsMember (regData, acc)) {
        regRefresh (regData);
      }
      n++; regData = fullList[n];
   }

   /* hack alert -- refesh adjbwindow too */
   recnRefresh (acc);
}

/********************************************************************\
\********************************************************************/

void accRefresh (Account *acc)
{
   /* avoid excess screen flicker with a two-phase refresh */
   MarkDirtyAllRegs (acc);
   RefreshAllRegs (acc);
}

/********************************************************************\
 * xaccDestroyLedgerWindow()
 * It is enought to call just XtDestroy Widget.  Any allocated
 * memory will be freed by the close callbacks.
\********************************************************************/

void
xaccDestroyLedgerWindow (Account *acc)
{
   LedgerWindow *regData;
   int n;

   /* find the single-account window for this account, if any */
   FIND_IN_LIST (LedgerWindow, regList, acc, lead_acct, regData);
   if (regData) XtDestroyWidget(regData->dialog);

   /* find the multiple-account window for this account, if any */
   FIND_IN_LIST (LedgerWindow, ledgerList, acc, lead_acct, regData);
   if (regData) XtDestroyWidget(regData->dialog);

   /* cruise throught the miscellanous account windows */
   n = 0;
   regData = fullList[n];
   while (regData) {
      int got_one;

      got_one = ledgerIsMember (regData, acc);
      /* if (got_one) XtDestroyWidget(regData->dialog);  */
      n++;
      regData = fullList[n];
   }
}

/********************************************************************\
 * closeLedgerWindow                                                   *
 *   frees memory allocated for an regWindow, and other cleanup     *
 *   stuff                                                          *
 *                                                                  *
 * Args:   mw - the widget that called us                           *
 *         cd - regData - the data struct for this register         *
 *         cb -                                                     *
 * Return: none                                                     *
\********************************************************************/
static void 
closeLedgerWindow( Widget mw, XtPointer cd, XtPointer cb )
{
  LedgerWindow *regData = (LedgerWindow *)cd;
  Account *acc = regData->lead_acct;
  
  /* Save any unsaved changes */
  xaccBRSaveRegEntry (regData->ledger);

  xaccDestroyBasicRegister (regData->ledger);
  
  /* whether this is a single or multi-account window, remove it */
  REMOVE_FROM_LIST (LedgerWindow, regList, acc, lead_acct);
  REMOVE_FROM_LIST (LedgerWindow, ledgerList, acc, lead_acct);

  ledgerListRemove (fullList, regData);

  free(regData);
}

/************************** END OF FILE *************************/
