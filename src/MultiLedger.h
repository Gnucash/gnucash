/*******************************************************************\
 * MultiLedger.h -- utilities for dealing with multiple             *
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

#ifndef __MULTI_LEDGER_H__
#define __MULTI_LEDGER_H__

#include "config.h"

#include <glib.h>

#include "Account.h"
#include "Query.h"
#include "splitreg.h"
#include "SplitLedger.h"
#include "Transaction.h"


/** Structures ******************************************************/

/* The xaccLedgerDisplay struct describes a single register/ledger
 * instance. */
typedef struct _xaccLedgerDisplay xaccLedgerDisplay;

struct _xaccLedgerDisplay
{
  Account *leader;               /* leading. "master" account, if any       */
  GList   *displayed_accounts;   /* The list of accounts shown here         */
  Query   *query;                /* query engine & filter for displaying    */

  SplitRegisterType type;        /* register display type, usually equal to *
                                  * account type, but not always.           */

  /* GUI related stuff */
  gboolean dirty;                /* dirty flag, non zero if redraw needed   */

  SplitRegister *reg;            /* main ledger window                      */
  gpointer gui_hook;             /* GUI-specific state                      */

  void (*destroy) (xaccLedgerDisplay *); /* destroy callback                */
  gncUIWidget (*get_parent) (xaccLedgerDisplay *); /* get parent widget     */
  void (*set_help) (xaccLedgerDisplay *, const char *); /* help string      */

  gint component_id;             /* id of ledger component                  */
};


/** Prototypes ******************************************************/

/* opens up a register window to display a single account */
xaccLedgerDisplay * xaccLedgerDisplaySimple (Account *account);

/* opens up a register window to display the parent account and all of
 * its children. */
xaccLedgerDisplay * xaccLedgerDisplayAccGroup (Account *account);

/* display list of accounts in a general ledger. */
xaccLedgerDisplay * xaccLedgerDisplayGeneral (Account *lead_account,
                                              GList *accounts,
                                              SplitRegisterType type,
                                              SplitRegisterStyle style);

/* redisplay/redraw all windows that contain any transactions that are
 * associated with the indicated account. */
void        xaccAccountDisplayRefresh (Account *account);
void        xaccAccGListDisplayRefresh (GList *accounts);

/* redisplay/redraw all windows that contain this transaction (or any
 * of its member splits). */
void        xaccTransDisplayRefresh (Transaction *trans);

/* redisplay/redraw only the indicated window. Both routines do same
 * thing, they differ only by the argument they take. */
void        xaccLedgerDisplayRefresh (xaccLedgerDisplay * ledger_display);

/* close the window */
void        xaccLedgerDisplayClose (xaccLedgerDisplay * ledger_display);

/* close all ledger windows containing this account. */
void        xaccDestroyLedgerDisplay (Account *account);

#endif /* __MULTI_LEDGER_H__ */

/************************** END OF FILE *************************/
