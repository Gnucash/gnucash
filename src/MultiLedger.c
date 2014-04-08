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
#include "SplitLedger.h"
#include "LedgerUtils.h"
#include "Transaction.h"
#include "util.h"


/** GLOBALS *********************************************************/
/* These are globals because they describe the state of the entire session.
 * The is, there must be only one instance of these per GUI session.
 */

static xaccLedgerDisplay **regList = NULL;     /* single-account registers */
static xaccLedgerDisplay **ledgerList = NULL;  /* multiple-account registers */
static xaccLedgerDisplay **fullList = NULL;    /* all registers */


/********************************************************************\
 * Ledger utilities                                                 *
 * Although these seem like they might be replacable with stock     *
 * list handling calls, I want to leave them like this for now,     *
 * since they manipulate global variables.  If this code ever       *
 * gets multi-threaded, access and edit of these globals will have  *
 * to be controlled with mutexes, and these utility routines        *
 * present a rather natural place for the locks to be placed.       *
\********************************************************************/

int 
ledgerListCount (xaccLedgerDisplay **list)
{
   int n = 0;
   if (!list) return 0;
   while (list[n]) n++;
   return n;
}

/* ------------------------------------------------------ */

xaccLedgerDisplay ** 
ledgerListAdd (xaccLedgerDisplay **oldlist, xaccLedgerDisplay *addreg)
{
   xaccLedgerDisplay **newlist;
   xaccLedgerDisplay *reg;
   int n;

   if (!addreg) return oldlist;

   n = ledgerListCount (oldlist);
   newlist = (xaccLedgerDisplay **) _malloc ((n+2) * sizeof (xaccLedgerDisplay *));

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
ledgerListRemove (xaccLedgerDisplay **list, xaccLedgerDisplay *delreg)
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
ledgerIsMember (xaccLedgerDisplay *reg, Account * acc)
{
   int n; 

   if (!acc) return 0;
   if (!reg) return 0;

   if (acc == reg->leader) return 1;

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

xaccLedgerDisplay *
xaccLedgerDisplaySimple (Account *acc)
  {
  xaccLedgerDisplay *retval;
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

  /* default to single-line display */
  reg_type |= REG_SINGLE_LINE;

  retval = xaccLedgerDisplayGeneral (acc, NULL, reg_type);
  return retval;
  }

/********************************************************************\
 * xaccLedgerDisplayAccGroup                                             *
 *   opens up a register window to display an account, and all      *
 *   of its children, in the same window                            *
 *                                                                  *
 * Args:   acc     - the account associated with this register      *
 * Return: regData - the register window instance                   *
\********************************************************************/

xaccLedgerDisplay *
xaccLedgerDisplayAccGroup (Account *acc)
  {
  xaccLedgerDisplay *retval;
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
      PERR (" xaccLedgerDisplayAccGroup(): unknown account type \n");
      _free (list);
      return NULL;
  }

  /* default to single-line display */
  ledger_type |= REG_SINGLE_LINE;

  retval = xaccLedgerDisplayGeneral (acc, list, ledger_type);

  if (list) _free (list);
  return retval;
  }

/********************************************************************\
 * xaccLedgerDisplayLedger                                               *
 *   opens up a ledger window for a list of accounts                *
 *                                                                  *
 * Args:   lead_acc - the account associated with this register     *
 *                     (may be null)                                *
 *         acc_list - the list of accounts to display in register   *
 *                     (may be null)                                *
 * Return: regData  - the register window instance                  *
\********************************************************************/

xaccLedgerDisplay *
xaccLedgerDisplayGeneral (Account *lead_acc, Account **acclist, int ledger_type)
  {
  xaccLedgerDisplay   *regData = NULL;

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
   * displayed have no particular relationshp to each other.  There
   * can be an arbitrary number of multiple-account leader-less
   * registers.
   */
  regData = NULL;
  if (lead_acc) {
     if (!acclist) {
       FETCH_FROM_LIST (xaccLedgerDisplay, regList, lead_acc, leader, regData);
     } else {
       FETCH_FROM_LIST (xaccLedgerDisplay, ledgerList, lead_acc, leader, regData);
     }
  }
  
  /* if regData is null, then no leader account was specified */
  if (!regData) {
    regData = (xaccLedgerDisplay *) malloc (sizeof (xaccLedgerDisplay));
  }

  regData->leader = lead_acc;
  regData->redraw = NULL;
  regData->destroy = NULL;
  regData->gui_hook = NULL;
  regData->dirty = 0;
  regData->balance = 0.0;
  regData->clearedBalance = 0.0;
  regData->reconciledBalance = 0.0;

  /* count the number of accounts we are supposed to display,
   * and then, store them. */
  regData->numAcc = accListCount (acclist);
  regData->displayed_accounts = accListCopy (acclist);
  regData->type = ledger_type;

  fullList = ledgerListAdd (fullList, regData);

  /******************************************************************\
   * The main register window itself                                *
  \******************************************************************/

  /* MallocBasicRegister will malloc & initialize the
   * register but doesn't do the gui init */
  regData->ledger = xaccMallocSplitRegister (ledger_type);
  
  regData->dirty = 1;
  xaccLedgerDisplayRefresh (regData);
  
  return regData;
}

/********************************************************************\
 * refresh only the indicated register window                       *
\********************************************************************/

void 
xaccLedgerDisplayRefresh (xaccLedgerDisplay *regData)
{
   int typo;

   /* If we don't really need the redraw, don't do it. */
   if (!(regData->dirty)) return;
   regData->dirty = 0;  /* mark clean */

   /* The leader account is used by the register gui to
    * assign a default source account for a "blank split"
    * that is attached to the bottom of the register.
    * The "blank split" is what the user edits to create 
    * new splits and get them into the system.
    */
   xaccSRLoadRegister (regData->ledger, 
                     xaccAccountGetSplitList (regData->leader),
                     regData->leader);


  /* hack alert -- this computation of totals is incorrect 
   * for multi-account ledgers */

  /* provide some convenience data for the ture GUI window.
   * If the GUI wants to display yet other stuff, its on its own.
   */
  xaccAccountRecomputeBalance(regData->leader);
  regData->balance = xaccAccountGetBalance (regData->leader);
  regData->clearedBalance = xaccAccountGetClearedBalance (regData->leader);
  regData->reconciledBalance = xaccAccountGetReconciledBalance (regData->leader);

  /* for income and expense acounts, we have to reverse
   * the meaning of balance, since, in a dual entry
   * system, income will show up as a credit to a
   * bank account, and a debit to the income account.
   * Thus, positive and negative are interchanged */
  typo = regData->type & REG_TYPE_MASK;
  if ((INCOME_REGISTER == typo) ||
      (EXPENSE_REGISTER == typo)) { 
    regData->balance = - (regData->balance);
    regData->clearedBalance = - (regData->clearedBalance);
    regData->reconciledBalance = - (regData->reconciledBalance);
  }

  /* OK, now tell this specific GUI window to redraw itself ... */
  if (regData->redraw) {
     (regData->redraw) (regData);
   }

}

/********************************************************************\
 * refresh only the indicated register window                       *
\********************************************************************/

void 
xaccRegisterRefresh (SplitRegister *splitreg)
{
   xaccLedgerDisplay *regData;
   int n;

   /* find the ledger which contains this register */
   n = 0; regData = fullList[n];
   while (regData) {
      if (splitreg == regData->ledger) {
        regData->dirty = 1;
        xaccLedgerDisplayRefresh (regData);
        return;
      }
      n++; regData = fullList[n];
   }
}

/********************************************************************\
 * sort of a quick hack involving the layout of the register.
\********************************************************************/

void 
xaccRegisterCountHack (SplitRegister *splitreg)
{
   xaccLedgerDisplay *regData;
   int n;

   /* find the ledger which contains this register */
   n = 0; regData = fullList[n];
   while (regData) {
      if (splitreg == regData->ledger) {
        xaccSRCountRows (splitreg, 
                      xaccAccountGetSplitList (regData->leader),
                      regData->leader);      
        return;
      }
      n++; regData = fullList[n];
   }
}

/********************************************************************\
 * mark dirty *all* register windows which contain this account      * 
\********************************************************************/

static void 
MarkDirtyAllRegs (Account *acc)
{
   xaccLedgerDisplay *regData;
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

static void 
RefreshAllRegs (Account *acc)
{
   xaccLedgerDisplay *regData;
   int n;

   if (!acc) return;

   /* find all registers which contain this account */
   n = 0; regData = fullList[n];
   while (regData) {
      if (ledgerIsMember (regData, acc)) {
        xaccLedgerDisplayRefresh (regData);
      }
      n++; regData = fullList[n];
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
xaccAccListDisplayRefresh (Account **acc_list)
{
   Account *acc;
   int i;

   /* avoid excess screen flicker with a two-phase refresh */
   i = 0; acc = acc_list[0];
   while (acc) {
      MarkDirtyAllRegs (acc);
      i++; acc = acc_list[i];
   }
   i = 0; acc = acc_list[0];
   while (acc) {
      RefreshAllRegs (acc);
      i++; acc = acc_list[i];
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
      Account *acc = xaccSplitGetAccount (split);
      MarkDirtyAllRegs (acc);
   }
   for (i=0; i<num_splits; i++) {
      Split *split = xaccTransGetSplit (trans, i);
      Account *acc = xaccSplitGetAccount (split);
      RefreshAllRegs (acc);
   }
}

/********************************************************************\
 * xaccDestroyLedgerDisplay()
\********************************************************************/

void
xaccDestroyLedgerDisplay (Account *acc)
{
   xaccLedgerDisplay *regData;
   int n;

   /* find the single-account window for this account, if any */
   FIND_IN_LIST (xaccLedgerDisplay, regList, acc, leader, regData);
   if (regData) {
      if (regData->destroy) { (regData->destroy) (regData); }
   } 

   /* find the multiple-account window for this account, if any */
   FIND_IN_LIST (xaccLedgerDisplay, ledgerList, acc, leader, regData);
   if (regData) {
      if (regData->destroy) { (regData->destroy) (regData); }
   } 

   /* cruise throught the miscellanous account windows */
   n = 0;
   regData = fullList[n];
   while (regData) {
      int got_one;

      got_one = ledgerIsMember (regData, acc);
      if (got_one) {
         if (regData->destroy) { (regData->destroy) (regData); }
      }
      n++;
      regData = fullList[n];
   }
}

/********************************************************************\
 * xaccLedgerDisplayClose                                           *
 *   frees memory allocated for an regWindow, and other cleanup     *
 *   stuff                                                          *
 *                                                                  *
 * Args:   mw - the widget that called us                           *
 *         cd - regData - the data struct for this register         *
 *         cb -                                                     *
 * Return: none                                                     *
\********************************************************************/
void 
xaccLedgerDisplayClose (xaccLedgerDisplay *regData)
{
  Account *acc = regData->leader;
  
  /* Save any unsaved changes */
  xaccSRSaveRegEntry (regData->ledger);

  /* refresh the register windows if there were changes */
  xaccSRRedrawRegEntry (regData->ledger);

  xaccDestroySplitRegister (regData->ledger);
  
  /* whether this is a single or multi-account window, remove it */
  REMOVE_FROM_LIST (xaccLedgerDisplay, regList, acc, leader);
  REMOVE_FROM_LIST (xaccLedgerDisplay, ledgerList, acc, leader);

  ledgerListRemove (fullList, regData);

  free(regData);
}

/************************** END OF FILE *************************/
