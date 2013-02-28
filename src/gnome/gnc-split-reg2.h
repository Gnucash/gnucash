/********************************************************************\
 * gnc-split-reg2.h -- A widget for the common register look-n-feel. *
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
#include "gnucash-sheet.h"
#include "gnc-split-reg.h"
#include "gnc-tree-view-split-reg.h"

#define GNC_SPLIT_REG2(obj)         G_TYPE_CHECK_INSTANCE_CAST( obj, gnc_split_reg2_get_type(), GNCSplitReg2 )
#define GNC_SPLIT_REG2_CLASS(klass) G_TYPE_CHECK_CLASS_CAST( klass, gnc_split_reg2_get_type(), GNCSplitReg2Class )
#define IS_GNC_SPLIT_REG2(obj)      G_TYPE_CHECK_INSTANCE_TYPE( obj, gnc_split_reg2_get_type() )

typedef struct _GNCSplitReg2 GNCSplitReg2;
typedef struct _GNCSplitReg2Class GNCSplitReg2Class;

struct _GNCSplitReg2
{
    /* The "parent" widget. */
    GtkVBox vbox;

    /* The containing window. */
    GtkWidget *window;
    gint width;
    gint height;

    GtkWidget *toolbar;
    GtkWidget *summarybar;

    GtkWidget *popup_menu;

    GtkWidget *edit_menu;
    GtkWidget *view_menu;
    GtkWidget *style_submenu;
    GtkWidget *sort_submenu;
    GtkWidget *action_menu;

    GtkWidget *double_line_check;

    GtkWidget *split_button;
    GtkWidget *split_menu_check;
    GtkWidget *split_popup_check;

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

    /** The actual sheet widget. **/
    GnucashRegister *reg;

    gint numRows;

    guint sort_type;

    gboolean read_only;
};

struct _GNCSplitReg2Class
{
    GtkVBoxClass parent_class;

    /* Signal defaults */

    void (*schedule_ent_cb) ( GNCSplitReg2 *w, gpointer user_data );

    void (*help_changed_cb) ( GNCSplitReg2 *w, gpointer user_data );
    void (*include_date_cb) ( GNCSplitReg2 *w, time64 date, gpointer user_data );
};

#ifdef skip // Coming from original gnc-split-reg.h
typedef enum
{
    SCHEDULE,
    SPLIT,
    SORT_ORDER_SUBMENU,
    STYLE_SUBMENU,
} GNC_SPLIT_REG2_ITEM;
#endif

/*FIXME Note sure about this == Coming from original gnc-split-reg.h */
typedef GNC_SPLIT_REG_ITEM GNC_SPLIT_REG2_ITEM;

/*FIXME Note sure about this == Coming from original gnc-split-reg.h */
#ifdef skip
/* Easy way to pass the sort-type
 * Note that this is STUPID -- we should be using parameter lists,
 * but this provides a simple case statement internally.  This should
 * probably not actually be exposed in the external interface....
 */
#define ENUM_LIST_SORTTYPE(_) \
  _(BY_NONE,) \
  _(BY_STANDARD,) \
  _(BY_DATE,) \
  _(BY_DATE_ENTERED,) \
  _(BY_DATE_RECONCILED,) \
  _(BY_NUM,) \
  _(BY_AMOUNT,) \
  _(BY_MEMO,) \
  _(BY_DESC,) \
  _(BY_ACTION,) \
  _(BY_NOTES,)

DEFINE_ENUM(SortType, ENUM_LIST_SORTTYPE)
AS_STRING_DEC(SortType, ENUM_LIST_SORTTYPE)
FROM_STRING_DEC(SortType, ENUM_LIST_SORTTYPE)
#endif


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
                              gboolean read_only );

/**
 * Returns the GncTreeView Split Register in effect for this GNCSplitReg.
 **/
GncTreeViewSplitReg *gnc_split_reg2_get_register (GNCSplitReg2 *gsr );

/**
 * Create and returns a summarybar for this GNCSplitReg.
 **/
GtkWidget *gsr2_create_summary_bar (GNCSplitReg2 *gsr );

/**
 * Gets/sets the sort-type of the GNCSplitReg.
 **/
SortType gnc_split_reg2_get_sort_type (GNCSplitReg2 *gsr );
void gnc_split_reg2_set_sort_type (GNCSplitReg2 *gsr, SortType t );

/**
 * Gets/sets the style of the GNCSplitReg.
 **/
void gnc_split_reg2_change_style (GNCSplitReg2 *gsr, SplitRegisterStyle2 style);

/**
 * Can return NULL if the indicated subwidget was not created.
 **/
GtkWidget *gnc_split_reg2_get_summarybar (GNCSplitReg2 *gsr );

void gnc_split_reg2_raise (GNCSplitReg2 *gsr );

/**
 * Callers can use this to determine if they need to reflect some "read-only"
 * status in the window which contains the GNCSplitReg.
 * @return TRUE if the register is read-only, FALSE if not.
 **/
gboolean gnc_split_reg2_get_read_only (GNCSplitReg2 *gsr );

/*
 * Create a transaction entry with given amount and date. One account is
 * specified, the other is undefined i.e. it defaults to orphan account.
 * Jump to the transaction entry in the register.
 * The purpose of this function to create an adjustment entry from the reconcile
 * window.
 */
void gnc_split_reg2_balancing_entry (GNCSplitReg2 *gsr, Account *account,
                                    time64 statement_date, gnc_numeric balancing_amount);

void gsr2_default_schedule_handler (GNCSplitReg2 *gsr, gpointer data );

void gnc_split_reg2_set_moved_cb (GNCSplitReg2 *gsr, GFunc cb, gpointer cb_data );

#endif /* GNC_SPLIT_REG2_H */
