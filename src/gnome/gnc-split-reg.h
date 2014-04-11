/********************************************************************\
 * gnc-split-reg.h -- A widget for the common register look-n-feel. *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1997-1998 Linas Vepstas <linas@linas.org>          *
 * Copyright (C) 1998 Rob Browning <rlb@cs.utexas.edu>              *
 * Copyright (C) 1999-2000 Dave Peticolas <dave@krondo.com>         *
 * Copyright (C) 2001 Gnumatic, Inc.                                *
 * Copyright (C) 2002,2006 Joshua Sled <jsled@asynchronous.org>     *
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


#ifndef GNC_SPLIT_REG_H
#define GNC_SPLIT_REG_H

#include <libguile.h>

#include "gnc-ledger-display.h"
#include "gnucash-sheet.h"

#define GNC_SPLIT_REG(obj)         GTK_CHECK_CAST( obj, gnc_split_reg_get_type(), GNCSplitReg )
#define GNC_SPLIT_REG_CLASS(klass) GTK_CHECK_CLASS_CAST( klass, gnc_split_reg_get_type(), GNCSplitRegClass )
#define IS_GNC_SPLIT_REG(obj)      GTK_CHECK_TYPE( obj, gnc_split_reg_get_type() )

typedef struct _GNCSplitReg GNCSplitReg;
typedef struct _GNCSplitRegClass GNCSplitRegClass;

struct _GNCSplitReg {
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

  GtkWidget * double_line_check;

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
  GNCLedgerDisplay *ledger;
  /** The actual sheet widget. **/
  GnucashRegister *reg;

  gint numRows;

  guint sort_type;

  gboolean read_only;
};

struct _GNCSplitRegClass {
  GtkVBoxClass parent_class;

  /* Signal defaults */
  void (*enter_ent_cb)    ( GNCSplitReg *w, gpointer user_data );
  void (*cancel_ent_cb)   ( GNCSplitReg *w, gpointer user_data );
  void (*delete_ent_cb)   ( GNCSplitReg *w, gpointer user_data );
  void (*reinit_ent_cb)   ( GNCSplitReg *w, gpointer user_data );
  void (*dup_ent_cb)      ( GNCSplitReg *w, gpointer user_data );
  void (*schedule_ent_cb) ( GNCSplitReg *w, gpointer user_data );
  void (*expand_ent_cb)   ( GNCSplitReg *w, gpointer user_data );
  void (*blank_cb)        ( GNCSplitReg *w, gpointer user_data );
  void (*jump_cb)         ( GNCSplitReg *w, gpointer user_data );
  void (*cut_cb)          ( GNCSplitReg *w, gpointer user_data );
  void (*cut_txn_cb)      ( GNCSplitReg *w, gpointer user_data );
  void (*copy_cb)         ( GNCSplitReg *w, gpointer user_data );
  void (*copy_txn_cb)     ( GNCSplitReg *w, gpointer user_data );
  void (*paste_cb)        ( GNCSplitReg *w, gpointer user_data );
  void (*paste_txn_cb)    ( GNCSplitReg *w, gpointer user_data );
  void (*void_txn_cb)     ( GNCSplitReg *w, gpointer user_data );
  void (*unvoid_txn_cb)   ( GNCSplitReg *w, gpointer user_data );
  void (*reverse_txn_cb)  ( GNCSplitReg *w, gpointer user_data );
  void (*help_changed_cb) ( GNCSplitReg *w, gpointer user_data );
  void (*include_date_cb) ( GNCSplitReg *w, time_t date, gpointer user_data );
};

typedef enum {
  ENTER,
  CANCEL,
  DELETE,
  REINIT,
  DUPLICATE,
  SCHEDULE,
  SPLIT,
  BLANK,
  JUMP,
  CUT,
  CUT_TXN,
  COPY,
  COPY_TXN,
  PASTE,
  PASTE_TXN,
  SORT_ORDER_SUBMENU,
  STYLE_SUBMENU,
} GNC_SPLIT_REG_ITEM;

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

/**
 * GTK-related; gets an identifier for the class of GNCSplitRegs.
 **/
GType gnc_split_reg_get_type(void);

/**
 * Creates and returns a GNCSplitReg.
 * @param ld            The GNCLedgerDisplay to use for display.
 * @param parent        The containing window.
 * @param numberOfLines The initial number of lines for the register.
 * @param read_only      If the contained register should be setup read-only.
 **/
GtkWidget* gnc_split_reg_new( GNCLedgerDisplay *ld,
                              GtkWindow *parent,
                              gint numberOfLines,
                              gboolean read_only );

/**
 * Returns the GnucashRegister in effect for this GNCSplitReg.
 **/
GnucashRegister *gnc_split_reg_get_register( GNCSplitReg *gsr );

/**
 * Create and returns a summarybar for this GNCSplitReg.
 **/
GtkWidget *gsr_create_summary_bar( GNCSplitReg *gsr );

/**
 * Gets/sets the sort-type of the GNCSplitReg.
 **/
SortType gnc_split_reg_get_sort_type( GNCSplitReg *gsr );
void gnc_split_reg_set_sort_type( GNCSplitReg *gsr, SortType t );

/**
 * Gets/sets the style of the GNCSplitReg.
 **/
void gnc_split_reg_change_style (GNCSplitReg *gsr, SplitRegisterStyle style);

/**
 * Can return NULL if the indicated subwidget was not created.
 **/
GtkWidget *gnc_split_reg_get_summarybar( GNCSplitReg *gsr );

/**
 * These will manipulate the in-GNCSplitReg state-reflecting widgets as
 * appropriate.
 **/
void gnc_split_reg_set_split_state( GNCSplitReg *gsr, gboolean split );
void gnc_split_reg_set_double_line( GNCSplitReg *gsr, gboolean doubleLine );

void gnc_split_reg_raise( GNCSplitReg *gsr );

/**
 * Callers can use this to determine if they need to reflect some "read-only"
 * status in the window which contains the GNCSplitReg.
 * @return TRUE if the register is read-only, FALSE if not.
 **/
gboolean gnc_split_reg_get_read_only( GNCSplitReg *gsr );

/*
 * Function to jump to various places in the register
 */
void gnc_split_reg_jump_to_blank (GNCSplitReg *gsr);
void gnc_split_reg_jump_to_split(GNCSplitReg *gsr, Split *split);
void gnc_split_reg_jump_to_split_amount(GNCSplitReg *gsr, Split *split);

/*
 * Create a transaction entry with given amount and date. One account is 
 * specified, the other is undefined i.e. it defaults to orphan account.  
 * Jump to the transaction entry in the register.
 * The purpose of this function to create an adjustment entry from the reconcile
 * window. 
 */
void gnc_split_reg_balancing_entry (GNCSplitReg *gsr, Account *account, 
    time_t statement_date, gnc_numeric balancing_amount);

void gsr_default_delete_handler( GNCSplitReg *gsr, gpointer data );
void gnc_split_reg_enter( GNCSplitReg *gsr, gboolean next_transaction );
void gsr_default_delete_handler( GNCSplitReg *gsr, gpointer data );
void gsr_default_reinit_handler( GNCSplitReg *gsr, gpointer data );
void gsr_default_expand_handler( GNCSplitReg *gsr, gpointer data );
void gsr_default_schedule_handler( GNCSplitReg *gsr, gpointer data );

void gnc_split_reg_set_moved_cb( GNCSplitReg *gsr, GFunc cb, gpointer cb_data );

#endif /* GNC_SPLIT_REG_H */
