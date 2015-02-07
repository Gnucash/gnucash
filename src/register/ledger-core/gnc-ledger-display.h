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

#include <glib.h>

#include "Account.h"
#include "Query.h"
#include "split-register.h"
#include "SchedXaction.h"
#include "Transaction.h"


/** Definitions *****************************************************/

/* The GNCLedgerDisplay struct describes a single register/ledger
 * instance.  It has a SplitRegister specially configured for
 * displaying the results of a Query.  It also stores the Query.  */
typedef struct gnc_ledger_display GNCLedgerDisplay;

typedef void (*GNCLedgerDisplayDestroy) (GNCLedgerDisplay *ld);
typedef GtkWidget *(*GNCLedgerDisplayGetParent) (GNCLedgerDisplay *ld);
typedef void (*GNCLedgerDisplaySetHelp) (GNCLedgerDisplay *ld,
        const char *help_str);

typedef enum
{
    LD_SINGLE,
    LD_SUBACCOUNT,
    LD_GL,
} GNCLedgerDisplayType;


/** Prototypes ******************************************************/

/* returns the 'lead' account of a ledger display, or NULL if none. */
Account * gnc_ledger_display_leader (GNCLedgerDisplay *ld);

GNCLedgerDisplayType gnc_ledger_display_type (GNCLedgerDisplay *ld);

/* get and set the user data associated with the ledger */
void gnc_ledger_display_set_user_data (GNCLedgerDisplay *ld,
                                       gpointer user_data);
gpointer gnc_ledger_display_get_user_data (GNCLedgerDisplay *ld);

/* set the handlers used by the ledger display */
void gnc_ledger_display_set_handlers (GNCLedgerDisplay *ld,
                                      GNCLedgerDisplayDestroy destroy,
                                      GNCLedgerDisplayGetParent get_parent);

/** Returns the parent of a given ledger display */
GtkWidget *gnc_ledger_display_get_parent( GNCLedgerDisplay *ld );

/* return the split register associated with a ledger display */
SplitRegister * gnc_ledger_display_get_split_register (GNCLedgerDisplay *ld);

/* opens up a register window to display a single account */
GNCLedgerDisplay * gnc_ledger_display_simple (Account *account);

/* opens up a register window to display the parent account and all of
 * its children. */
GNCLedgerDisplay * gnc_ledger_display_subaccounts (Account *account);

/* opens up a general journal window */
GNCLedgerDisplay * gnc_ledger_display_gl (void);

/**
 * Displays a template ledger.
 * This lists template Splits from the given ScheduledTransaction.
 *
 * Really, requires a GList of scheduled transactions and kvp-frame
 * data.
 **/
GNCLedgerDisplay * gnc_ledger_display_template_gl (char *id);

/* display a general journal for an arbitrary query */
GNCLedgerDisplay * gnc_ledger_display_query (Query *query,
        SplitRegisterType type,
        SplitRegisterStyle style);

/* Set the query used for a register. */
void gnc_ledger_display_set_query (GNCLedgerDisplay *ledger_display,
                                   Query *q);

/* return the query associated with a ledger */
Query * gnc_ledger_display_get_query (GNCLedgerDisplay *ld);

/* If the given ledger display still exists, return it. Otherwise,
 * return NULL */
GNCLedgerDisplay * gnc_ledger_display_find_by_query (Query *q);

/* redisplay/redraw only the indicated window. Both routines do same
 * thing, they differ only by the argument they take. */
void gnc_ledger_display_refresh (GNCLedgerDisplay * ledger_display);
void gnc_ledger_display_refresh_by_split_register (SplitRegister *reg);

/* close the window */
void gnc_ledger_display_close (GNCLedgerDisplay * ledger_display);

/* Returns a boolean of whether this display should be single or double lined
 * mode by default */
gboolean gnc_ledger_display_default_double_line (GNCLedgerDisplay *gld);

#endif
