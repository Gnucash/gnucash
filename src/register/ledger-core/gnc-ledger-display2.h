/********************************************************************\
 * gnc-ledger-display.h -- utilities for dealing with multiple      *
 *                         register/ledger windows in GnuCash       *
 *                                                                  *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1997, 1998 Linas Vepstas                           *
 * Copyright (C) 2001 Linux Developers Group                        *
 * Copyright (C) 2012 Robert Fewell                                 *
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

#ifndef GNC_LEDGER_DISPLAY2_H
#define GNC_LEDGER_DISPLAY2_H

#include <glib.h>

#include "Account.h"
#include "Query.h"

#include "split-register.h" 

#include "SchedXaction.h"
#include "Transaction.h"
#include "gnc-tree-model-split-reg.h"
#include "gnc-tree-view-split-reg.h"

/** Definitions *****************************************************/

/* The GNCLedgerDisplay2 struct describes a single register/ledger
 * instance.  It has a SplitRegister specially configured for
 * displaying the results of a Query.  It also stores the Query.  */
typedef struct gnc_ledger_display2 GNCLedgerDisplay2;

typedef void (*GNCLedgerDisplay2Destroy) (GNCLedgerDisplay2 *ld);
typedef GtkWidget *(*GNCLedgerDisplay2GetParent) (GNCLedgerDisplay2 *ld);
typedef void (*GNCLedgerDisplay2SetHelp) (GNCLedgerDisplay2 *ld,
        const char *help_str);


typedef enum
{
    LD2_SINGLE,
    LD2_SUBACCOUNT,
    LD2_GL,
} GNCLedgerDisplay2Type;


/** Prototypes ******************************************************/

/* returns the 'lead' account of a ledger display, or NULL if none. */
Account * gnc_ledger_display2_leader (GNCLedgerDisplay2 *ld);

GNCLedgerDisplay2Type gnc_ledger_display2_type (GNCLedgerDisplay2 *ld);

/* get and set the user data associated with the ledger */
void gnc_ledger_display2_set_user_data (GNCLedgerDisplay2 *ld,
                                       gpointer user_data);
gpointer gnc_ledger_display2_get_user_data (GNCLedgerDisplay2 *ld);

/* set the handlers used by the ledger display */
void gnc_ledger_display2_set_handlers (GNCLedgerDisplay2 *ld,
                                      GNCLedgerDisplay2Destroy destroy,
                                      GNCLedgerDisplay2GetParent get_parent);

/* Set and Get the tree view used by the ledger display */
void gnc_ledger_display2_set_split_view_register (GNCLedgerDisplay2 *ld, GncTreeViewSplitReg *view);
GncTreeViewSplitReg * gnc_ledger_display2_get_split_view_register (GNCLedgerDisplay2 *ld);

void gnc_ledger_display2_set_split_view_refresh (GNCLedgerDisplay2 *ld, gboolean ok);

/** Returns the parent of a given ledger display */
GtkWidget *gnc_ledger_display2_get_parent( GNCLedgerDisplay2 *ld );

/* return the split register associated with a ledger display */
GncTreeModelSplitReg * gnc_ledger_display2_get_split_model_register (GNCLedgerDisplay2 *ld);

/* opens up a register window to display a single account */
GNCLedgerDisplay2 * gnc_ledger_display2_simple (Account *account);

/* opens up a register window to display the parent account and all of
 * its children. */
GNCLedgerDisplay2 * gnc_ledger_display2_subaccounts (Account *account);

/* opens up a general journal window */
GNCLedgerDisplay2 * gnc_ledger_display2_gl (void);

/**
 * Displays a template ledger.
 * This lists template Splits from the given ScheduledTransaction.
 *
 * Really, requires a GList of scheduled transactions and kvp-frame
 * data.
 **/
GNCLedgerDisplay2 * gnc_ledger_display2_template_gl (char *id);

/* display a general journal for an arbitrary query */
GNCLedgerDisplay2 * gnc_ledger_display2_query (Query *query,
        SplitRegisterType2 type,
        SplitRegisterStyle2 style);

/* Set the query used for a register. */
void gnc_ledger_display2_set_query (GNCLedgerDisplay2 *ledger_display,
                                   Query *q);

/* return the query associated with a ledger */
Query * gnc_ledger_display2_get_query (GNCLedgerDisplay2 *ld);

/* If the given ledger display still exists, return it. Otherwise,
 * return NULL */
GNCLedgerDisplay2 * gnc_ledger_display2_find_by_query (Query *q);

/* redisplay/redraw only the indicated window. Both routines do same
 * thing, they differ only by the argument they take. */
void gnc_ledger_display2_refresh (GNCLedgerDisplay2 * ledger_display);
void gnc_ledger_display2_refresh_by_split_register (GncTreeModelSplitReg *model);

/* This is used to load the register for the schedule */
void gnc_ledger_display2_refresh_sched (GNCLedgerDisplay2 *ld, GList *splits);

/* Refilter the register */
void gnc_ledger_display2_refilter (GNCLedgerDisplay2 *ld);

/* close the window */
void gnc_ledger_display2_close (GNCLedgerDisplay2 * ledger_display);

/* Returns a boolean of whether this display should be single or double lined
 * mode by default */
gboolean gnc_ledger_display2_default_double_line (GNCLedgerDisplay2 *gld);

#endif
