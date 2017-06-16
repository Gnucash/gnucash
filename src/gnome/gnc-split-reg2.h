/********************************************************************\
 * gnc-split-reg2.h -- A widget for the common register look-n-feel.*
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1997-1998 Linas Vepstas <linas@linas.org>          *
 * Copyright (C) 1998 Rob Browning <rlb@cs.utexas.edu>              *
 * Copyright (C) 1999-2000 Dave Peticolas <dave@krondo.com>         *
 * Copyright (C) 2001 Gnumatic, Inc.                                *
 * Copyright (C) 2002,2006 Joshua Sled <jsled@asynchronous.org>     *
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
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/


#ifndef GNC_SPLIT_REG2_H
#define GNC_SPLIT_REG2_H

#include "gnc-ledger-display2.h"
#include "gnc-split-reg.h"
#include "gnc-tree-view-split-reg.h"
/** @ingroup Register
 * @addtogroup Register2
 * @{
 */
/** @file gnc-split-reg2.h
 */
#define GNC_SPLIT_REG2(obj)         G_TYPE_CHECK_INSTANCE_CAST( obj, gnc_split_reg2_get_type(), GNCSplitReg2 )
#define GNC_SPLIT_REG2_CLASS(klass) G_TYPE_CHECK_CLASS_CAST( klass, gnc_split_reg2_get_type(), GNCSplitReg2Class )
#define IS_GNC_SPLIT_REG2(obj)      G_TYPE_CHECK_INSTANCE_TYPE( obj, gnc_split_reg2_get_type() )

typedef struct _GNCSplitReg2 GNCSplitReg2;
typedef struct _GNCSplitReg2Class GNCSplitReg2Class;

struct _GNCSplitReg2
{
    /* The "parent" widget. */
    GtkBox vbox;

    /* The containing window. */
    GtkWidget *window;
    GtkWidget *scroll_bar;
    GtkAdjustment *scroll_adj;

    GtkWidget *toolbar;
    GtkWidget *summarybar;

    /* Summary Bar Labels */
    GtkWidget *balance_label;
    GtkWidget *cleared_label;
    GtkWidget *reconciled_label;
    GtkWidget *future_label;
    GtkWidget *projectedminimum_label;
    GtkWidget *shares_label;
    GtkWidget *value_label;

    /** The current ledger display. **/
    GNCLedgerDisplay2 *ledger;

    gint numRows;

    gboolean read_only;
};

struct _GNCSplitReg2Class
{
    GtkBoxClass parent_class;

    /* Signal defaults */
    void (*help_changed) (GNCSplitReg2 *w, gpointer user_data);
};

/*FIXME Note sure about this == Coming from original gnc-split-reg.h */
typedef GNC_SPLIT_REG_ITEM GNC_SPLIT_REG2_ITEM;

/**
 * GTK-related; gets an identifier for the class of GNCSplitRegs.
 **/
GType gnc_split_reg2_get_type (void);

/**
 * Creates and returns a GNCSplitReg.
 * @param ld            The GNCLedgerDisplay to use for display.
 * @param parent        The containing window.
 * @param numberOfLines The initial number of lines for the register.
 * @param read_only      If the contained register should be setup read-only.
 **/
GtkWidget* gnc_split_reg2_new (GNCLedgerDisplay2 *ld,
                              GtkWindow *parent,
                              gint numberOfLines,
                              gboolean read_only);

/**
 * Returns the GncTreeView Split Register in effect for this GNCSplitReg.
 **/
GncTreeViewSplitReg *gnc_split_reg2_get_register (GNCSplitReg2 *gsr);

/**
 * Create and returns a summarybar for this GNCSplitReg.
 **/
GtkWidget *gnc_split_reg2_create_summary_bar (GNCSplitReg2 *gsr);

/**
 * Gets/sets the style of the GNCSplitReg.
 **/
void gnc_split_reg2_change_style (GNCSplitReg2 *gsr, SplitRegisterStyle2 style);

/**
 * Can return NULL if the indicated subwidget was not created.
 **/
GtkWidget *gnc_split_reg2_get_summarybar (GNCSplitReg2 *gsr);

/**
 * Jump to split.
 **/
void gnc_split_reg2_jump_to_split (GNCSplitReg2 *gsr, Split *split);

/**
 * Move the cursor to the split in the non-blank amount column.
 **/
void gnc_split_reg2_jump_to_split_amount (GNCSplitReg2 *gsr, Split *split);


/**
 * Raise an existing register window to the front.
 **/
void gnc_split_reg2_raise (GNCSplitReg2 *gsr);

/**
 * Callers can use this to determine if they need to reflect some "read-only"
 * status in the window which contains the GNCSplitReg.
 * @return TRUE if the register is read-only, FALSE if not.
 **/
gboolean gnc_split_reg2_get_read_only (GNCSplitReg2 *gsr);

/*
 * Create a transaction entry with given amount and date. One account is
 * specified, the other is undefined i.e. it defaults to orphan account.
 * Jump to the transaction entry in the register.
 * The purpose of this function to create an adjustment entry from the reconcile
 * window.
 */
void gnc_split_reg2_balancing_entry (GNCSplitReg2 *gsr, Account *account,
                                    time64 statement_date, gnc_numeric balancing_amount);


void gnc_split_reg2_set_moved_cb (GNCSplitReg2 *gsr, GFunc cb, gpointer cb_data);
/** @} */
#endif /* GNC_SPLIT_REG2_H */
