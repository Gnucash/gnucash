/********************************************************************\
 * gnc-split-reg.h -- A widget for the common register look-n-feel. *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1997-1998 Linas Vepstas <linas@linas.org>          *
 * Copyright (C) 1998 Rob Browning <rlb@cs.utexas.edu>              *
 * Copyright (C) 1999-2000 Dave Peticolas <dave@krondo.com>         *
 * Copyright (C) 2001 Gnumatic, Inc.                                *
 * Copyright (C) 2002 Joshua Sled <jsled@asynchronous.org>          *
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/


/**
 * Another take at the gnc-reg-widget.
 * 2002.10.27 -- jsled
 *
 * To be explained:
 * . inserting controls
 *   . menus, toolbar
 *     . gtk_{menu,toolbar}_{append,prepend,insert} is good; callers need to know indexes
 *       . gint gnc_reg_widget_get_toolbar_index( GNCRegWidget, GNC_REG_WIDGET_ITEM )
 *       . gint gnc_reg_widget_get_menu_index   ( GNCRegWidget, GNC_REG_WIDGET_ITEM )
 *       . gint gnc_reg_widget_get_popup_index  ( GNCRegWidget, GNC_REG_WIDGET_ITEM )
 *   
 * . created status-display widgets [checkboxes in menus, &c.]
 *   . i/f to changing?
 *     . gnc_reg_widget_set_split_state( GNCRegWidget, gboolean split )
 *     . gnc_reg_widget_set_double_line( GNCRegWidget, gboolean doubleLine )
 *
 * Questionable Features:
 * . File
 *   . new account
 *   . print
 *   . print check
 *   . save (as)...
 *   . close
 *   . exit
 * . view
 *   . date range
 * . edit
 *   . find
 * . actions
 *   . transfer
 *   . reconcile
 *   . stock split
 *   . check and repair
 * . reports
 * . tools
 **/

#ifndef GNC_SPLIT_REG_H
#define GNC_SPLIT_REG_H

#include <libguile.h>
#include <gtk/gtk.h>
#include "config.h"

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

  SCM toolbar_change_callback_id;
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

  GtkWidget *balance_label;
  GtkWidget *cleared_label;
  GtkWidget *reconciled_label;
  GtkWidget *future_label;
  GtkWidget *shares_label;
  GtkWidget *value_label;

  /** The current ledger display. **/
  GNCLedgerDisplay *ledger;
  /** The actual sheet widget. **/
  GnucashRegister *reg;

  gint numRows;
  gint createFlags;
  gint disallowedCaps;

  gint sort_type;

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
  void (*help_changed_cb) ( GNCSplitReg *w, gpointer user_data );
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

/* Easy way to pass the sort-type */
typedef enum {
  BY_NONE = 0,
  BY_STANDARD,
  BY_DATE,
  BY_DATE_ENTERED,
  BY_DATE_RECONCILED,
  BY_NUM,
  BY_AMOUNT,
  BY_MEMO,
  BY_DESC
} SortType;

/**
 * Flags for creation-time selection of what subwidgets are to be created.
 **/
#define CREATE_TOOLBAR    (1 << 0)
#define CREATE_MENUS      (1 << 1)
#define CREATE_POPUP      (1 << 2)
#define CREATE_SUMMARYBAR (1 << 3)

/**
 * Flags for various capabilities of the GNCSplitReg; these are used to
 * disable specific functionality.
 **/
#define CAP_READ_ONLY (1 << 0)  /**< A read-only register. **/
#define CAP_DELETE    (1 << 1)  /**< Deleting items. **/
#define CAP_JUMP      (1 << 2)  /**< Jumping to the related transaction. **/
#define CAP_SCHEDULE  (1 << 3)  /**< Scheduling transactions. **/

/**
 * GTK-related; gets an identifier for the class of GNCSplitRegs.
 **/
guint gnc_split_reg_get_type(void);

/**
 * Creates and returns a GNCSplitReg.
 * @param ld            The GNCLedgerDisplay to use for display.
 * @param parent        The containing window.
 * @param numberOfLines The initial number of lines for the register.
 * @param createFlags   A set of flags for the sub-widgets to create.
 * @param disallowCaps  A set of flags for capabilities which should be
 *                      disallowed.
 **/
GtkWidget* gnc_split_reg_new( GNCLedgerDisplay *ld,
                              GtkWindow *parent,
                              gint numberOfLines,
                              gint createFlags,
                              gint disallowCaps );

/**
 * Returns the GnucashRegister in effect for this GNCSplitReg.
 **/
GnucashRegister *gnc_split_reg_get_register( GNCSplitReg *gsr );

/**
 * Gets/sets the sort-type of the GNCSplitReg.
 **/
SortType gnc_split_reg_get_sort_type( GNCSplitReg *gsr );
void gnc_split_reg_set_sort_type( GNCSplitReg *gsr, SortType t );

/**
 * Retreives the various menus created by the GNCSplitReg.  Callers may want
 * to put these in a more traditional menu bar, for instance.
 **/
GtkWidget *gnc_split_reg_get_edit_menu       ( GNCSplitReg *gsr );
GtkWidget *gnc_split_reg_get_view_menu       ( GNCSplitReg *gsr );
GtkWidget *gnc_split_reg_get_style_menu      ( GNCSplitReg *gsr );
GtkWidget *gnc_split_reg_get_sort_menu       ( GNCSplitReg *gsr );
GtkWidget *gnc_split_reg_get_action_menu     ( GNCSplitReg *gsr );

/**
 * Can return NULL if the indicated subwidget was not created.
 **/
GtkWidget *gnc_split_reg_get_toolbar   ( GNCSplitReg *gsr );
GtkWidget *gnc_split_reg_get_summarybar( GNCSplitReg *gsr );
GtkWidget *gnc_split_reg_get_popup     ( GNCSplitReg *gsr );

/**
 * These will manipulate the in-GNCSplitReg state-reflecting widgets as
 * appropriate.
 **/
void gnc_split_reg_set_split_state( GNCSplitReg *gsr, gboolean split );
void gnc_split_reg_set_double_line( GNCSplitReg *gsr, gboolean doubleLine );

/**
 * Convenience function for users.  Returns the popup menu containing what
 * would normally be the menu-bar-placed menu items.  Callers may not have a
 * menu bar, but wish to provide full functionality.
 *
 * The menu-bar items will be at the bottom of the popup menu, seperated.
 * The menu-bar items will be created if they were not originally created.
 **/
GtkWidget *gnc_split_reg_get_popup_extended( GNCSplitReg *gsr );

gboolean gnc_split_reg_check_close( GNCSplitReg *gsr );

/*
 * Function to jump to various places in the register
 */
void gnc_split_reg_jump_to_blank (GNCSplitReg *gsr);
void gnc_split_reg_jump_to_split(GNCSplitReg *gsr, Split *split);
void gnc_split_reg_jump_to_split_amount(GNCSplitReg *gsr, Split *split);

#endif /* GNC_SPLIT_REG_H */
