/********************************************************************\
 * gnc-ledger-display.h -- utilities for dealing with multiple      *
 *                         register/ledger windows in GnuCash       *
 *                                                                  *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1997, 1998 Linas Vepstas                           *
 * Copyright (C) 2001 Linux Developers Group                        *
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

#ifndef GNC_LEDGER_DISPLAY_H
#define GNC_LEDGER_DISPLAY_H

#include "config.h"

#include <glib.h>

#include "Account.h"
#include "Query.h"
#include "split-register.h"
#include "SchedXaction.h"
#include "Transaction.h"


/** Definitions *****************************************************/

/* The xaccLedgerDisplay struct describes a single register/ledger
 * instance. */
typedef struct _xaccLedgerDisplay xaccLedgerDisplay;

typedef void (*LedgerDisplayDestroy) (xaccLedgerDisplay *ld);
typedef gncUIWidget (*LedgerDisplayGetParent) (xaccLedgerDisplay *ld);
typedef void (*LedgerDisplaySetHelp) (xaccLedgerDisplay *ld,
                                      const char *help_str);

typedef enum
{
  LD_SINGLE,
  LD_SUBACCOUNT,
  LD_GL,
  LD_TEMPLATE,
} LedgerDisplayType;


/** Prototypes ******************************************************/

/* returns the 'lead' account of a ledger display, or NULL if none. */
Account * xaccLedgerDisplayLeader (xaccLedgerDisplay *ld);

LedgerDisplayType xaccLedgerDisplayType (xaccLedgerDisplay *ld);

/* get and set the user data associated with the ledger */
void xaccLedgerDisplaySetUserData (xaccLedgerDisplay *ld, gpointer user_data);
gpointer xaccLedgerDisplayGetUserData (xaccLedgerDisplay *ld);

/* set the handlers used by the ledger display */
void xaccLedgerDisplaySetHandlers (xaccLedgerDisplay *ld,
                                   LedgerDisplayDestroy destroy,
                                   LedgerDisplayGetParent get_parent);

/* return the split register associated with a ledger display */
SplitRegister * xaccLedgerDisplayGetSR (xaccLedgerDisplay *ld);

/* opens up a register window to display a single account */
xaccLedgerDisplay * xaccLedgerDisplaySimple (Account *account);

/* opens up a register window to display the parent account and all of
 * its children. */
xaccLedgerDisplay * xaccLedgerDisplayAccGroup (Account *account);

xaccLedgerDisplay * xaccLedgerDisplayGL (void);
xaccLedgerDisplay * xaccLedgerDisplayGLTemplate (char *id);

/**
 * Displays a template ledger.
 * This lists template Splits from the given ScheduledTransaction.
 *
 * Really, requires a GList of scheduled transactions and kvp-frame
 * data.
 **/
xaccLedgerDisplay * xaccLedgerDisplayTemplateGL (char *id);

/* display a general ledger for an arbitrary query */
xaccLedgerDisplay * xaccLedgerDisplayQuery (Query *query,
                                            SplitRegisterType type,
                                            SplitRegisterStyle style);

/* Set the query used for a register. */
void xaccLedgerDisplaySetQuery (xaccLedgerDisplay *ledger_display, Query *q);

/* return the query associated with a ledger */
Query * xaccLedgerDisplayGetQuery (xaccLedgerDisplay *ld);

/* If the given ledger display still exists, return it. Otherwise,
 * return NULL */
xaccLedgerDisplay * xaccFindGeneralLedgerByQuery (Query *q);

/* redisplay/redraw only the indicated window. Both routines do same
 * thing, they differ only by the argument they take. */
void        xaccLedgerDisplayRefresh (xaccLedgerDisplay * ledger_display);
void        xaccLedgerDisplayRefreshByReg (SplitRegister *reg);

/* close the window */
void        xaccLedgerDisplayClose (xaccLedgerDisplay * ledger_display);

/* close all ledger windows containing this account. */
void        xaccDestroyLedgerDisplay (Account *account);

#endif
