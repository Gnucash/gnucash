/********************************************************************\
 * split-register.h -- split register api                           *
 * Copyright (C) 1998-2000 Linas Vepstas                            *
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
 *                                                                  *
\********************************************************************/

#ifndef SPLIT_REGISTER_H
#define SPLIT_REGISTER_H

#include <glib.h>

#include "Group.h"
#include "Transaction.h"
#include "gnc-common.h"
#include "table-allgui.h"


/** Datatypes *******************************************************/

/* Register types.
 * "registers" are single-account display windows.
 * "ledgers" are multiple-account display windows */
typedef enum
{
  BANK_REGISTER,
  CASH_REGISTER,
  ASSET_REGISTER,
  CREDIT_REGISTER,
  LIABILITY_REGISTER,
  INCOME_REGISTER,
  EXPENSE_REGISTER,
  EQUITY_REGISTER,
  STOCK_REGISTER,
  CURRENCY_REGISTER,
  NUM_SINGLE_REGISTER_TYPES,

  GENERAL_LEDGER = NUM_SINGLE_REGISTER_TYPES,
  INCOME_LEDGER,
  PORTFOLIO_LEDGER,
  SEARCH_LEDGER,

  NUM_REGISTER_TYPES
} SplitRegisterType;

typedef enum
{
  REG_STYLE_LEDGER,
  REG_STYLE_AUTO_LEDGER,
  REG_STYLE_JOURNAL
} SplitRegisterStyle;

/* Cell Names. T* cells are transaction summary cells */
#define ACTN_CELL  "action"
#define BALN_CELL  "balance"
#define CRED_CELL  "credit"
#define DATE_CELL  "date"
#define DEBT_CELL  "debit"
#define DESC_CELL  "description"
#define FCRED_CELL "credit-formula"
#define FDEBT_CELL "debit formula"
#define MEMO_CELL  "memo"
#define MXFRM_CELL "transfer"
#define NOTES_CELL "notes"
#define NUM_CELL   "num"
#define PRIC_CELL  "price"
#define RECN_CELL  "reconcile"
#define SHRS_CELL  "shares"
#define TBALN_CELL "trans-balance"
#define TCRED_CELL "trans-credit"
#define TDEBT_CELL "trans-debit"
#define TSHRS_CELL "trans-shares"
#define XFRM_CELL  "account"

/* Cursor Names */
#define CURSOR_SINGLE_LEDGER  "cursor-single-ledger"
#define CURSOR_DOUBLE_LEDGER  "cursor-double-ledger"
#define CURSOR_SINGLE_JOURNAL "cursor-single-journal"
#define CURSOR_DOUBLE_JOURNAL "cursor-double-journal"
#define CURSOR_SPLIT          "cursor-split"

/* Types of cursors */
typedef enum
{
  CURSOR_CLASS_NONE = -1,
  CURSOR_CLASS_SPLIT,
  CURSOR_CLASS_TRANS,
  NUM_CURSOR_CLASSES
} CursorClass;

typedef struct split_register_colors
{
  guint32 header_bg_color;

  guint32 primary_bg_color;
  guint32 secondary_bg_color;

  guint32 primary_active_bg_color;
  guint32 secondary_active_bg_color;

  guint32 split_bg_color;
  guint32 split_active_bg_color;

  gboolean double_alternate_virt;
} SplitRegisterColors;


typedef struct split_register SplitRegister;
typedef struct sr_info SRInfo;

struct split_register
{
  /* the table itself that implements the underlying GUI. */
  Table * table;

  SplitRegisterType type;
  SplitRegisterStyle style;

  gboolean use_double_line;
  gboolean is_template;

  /* private data; outsiders should not access this */
  SRInfo * sr_info;
};

/* Callback function type */
typedef gncUIWidget (*SRGetParentCallback) (gpointer user_data);


/** Prototypes ******************************************************/

/* Create and return a new split register. */
SplitRegister * gnc_split_register_new (SplitRegisterType type,
                                        SplitRegisterStyle style,
                                        gboolean use_double_line,
                                        gboolean is_template);

/* Configure the split register. */
void gnc_split_register_config (SplitRegister *reg,
                                SplitRegisterType type,
                                SplitRegisterStyle style,
                                gboolean use_double_line);

/* Destroy the split register. */
void gnc_split_register_destroy (SplitRegister *reg);

/* Set the template account used by template registers */
void gnc_split_register_set_template_account (SplitRegister *reg,
                                              Account *template_account);

/* Returns the class of the current cursor */
CursorClass gnc_split_register_get_current_cursor_class (SplitRegister *reg);

/* Returns the class of the cursor at the given virtual cell location. */
CursorClass gnc_split_register_get_cursor_class
                                              (SplitRegister *reg,
                                               VirtualCellLocation vcell_loc);

/* Sets the user data and callback hooks for the register. */
void gnc_split_register_set_data (SplitRegister *reg, gpointer user_data,
                                  SRGetParentCallback get_parent);

/* Returns the transaction which is the parent of the current split. */
Transaction * gnc_split_register_get_current_trans (SplitRegister *reg);

/* The xaccSRGetCurrentSplit() method returns the split at which 
 *    the cursor is currently located. */
Split * xaccSRGetCurrentSplit (SplitRegister *reg);

/* The xaccSRGetBlankSplit() method returns the blank split or
 *    NULL if there is none. */
Split * xaccSRGetBlankSplit (SplitRegister *reg);

/* The xaccSRGetSplitVirtLoc() method searches the split register
 *    for the given split. If found, it returns TRUE and vcell_loc
 *    is set to the location of the split. Otherwise, the method
 *    returns FALSE. */
gboolean xaccSRGetSplitVirtLoc (SplitRegister *reg, Split *split,
                                VirtualCellLocation *vcell_loc);

/* The xaccSRGetSplitAmountVirtLoc() method searches the split
 *    register for the given split. If found, it returns TRUE and
 *    virt_loc is set to the location of either the debit or credit
 *    column in the split, whichever one is non-blank. Otherwise,
 *    the method returns FALSE. */
gboolean xaccSRGetSplitAmountVirtLoc (SplitRegister *reg, Split *split,
                                      VirtualLocation *virt_loc);

/* The xaccSRDuplicateCurrent() method duplicates either the current
 *    transaction or the current split depending on the register mode
 *    and cursor position. Returns the split just created, or the
 *    'main' split of the transaction just created, or NULL if
 *    nothing happened. */
Split * xaccSRDuplicateCurrent (SplitRegister *reg);

/* The xaccSRCopyCurrent() method makes a copy of the current entity,
 *    either a split or a transaction, so that it can be pasted later. */
void    xaccSRCopyCurrent  (SplitRegister *reg);

/* The xaccSRCutCurrent() method is equivalent to copying the current
 *    entity and the deleting it with the approriate delete method. */
void    xaccSRCutCurrent   (SplitRegister *reg);

/* The xaccSRPasteCurrent() method pastes a previous copied entity
 *    onto the current entity, but only if the copied entity and the
 *    current entity are of the same type. */
void    xaccSRPasteCurrent (SplitRegister *reg);

/* The xaccSRDeleteCurrentSplit() method deletes the split associated
 *    with the current cursor, if both are non-NULL. If successful, all
 *    affected account windows are refreshed. Deleting the blank split
 *    just clears the cursor values. */
void    xaccSRDeleteCurrentSplit (SplitRegister *reg);

/* The xaccSRDeleteCurrentTrans() method deletes the transaction
 *    associated with the current cursor, if both are non-NULL.
 *    If successful, all affected account windows are refreshed. */
void    xaccSRDeleteCurrentTrans (SplitRegister *reg);

/* The xaccSREmptyCurrentTrans() method deletes the non-transaction
 *    splits associated wih the current cursor, if both are non-NULL.
 *    If successful, all affected account windows are refreshed. */
void    xaccSREmptyCurrentTrans  (SplitRegister *reg);

/* The xaccSRCancelCursorSplitChanges() method cancels any changes made
 *    to the current cursor, reloads the cursor from the engine, reloads
 *    the table from the cursor, and updates the GUI. The change flags
 *    are cleared. */
void    xaccSRCancelCursorSplitChanges (SplitRegister *reg);

/* The xaccSRCancelCursorTransChanges() method cancels any changes made
 *    to the current pending transaction, reloads the table from the engine,
 *    and updates the GUI. The change flags are cleared. */
void    xaccSRCancelCursorTransChanges (SplitRegister *reg);

/* The xaccSRLoadRegister() subroutine will copy transaction
 *    information from a list of splits to the rows of the
 *    register GUI.  The third argument, default_source_acc,
 *    will be used to initialize the source account of a new,
 *    blank split appended to the tail end of the register.
 *    This "blank split" is the place where the user can 
 *    edit info to create new tranasactions. */
void    xaccSRLoadRegister (SplitRegister *reg, GList * slist,
                            Account *default_source_acc);

/* The xaccSRSaveRegEntry() method will copy the contents 
 *    from the cursor to a split.  The split/transaction
 *    that is updated is the one associated with the current 
 *    cursor (register entry) position. If the do_commit flag
 *    is set, the transaction will also be committed. If it is
 *    the blank transaction, and the do_commit flag is set,
 *    a refresh will result in a new blank transaction.
 *    The method returns TRUE if something was changed. */
gboolean xaccSRSaveRegEntry (SplitRegister *reg, gboolean do_commit);
gboolean xaccSRTemplateSaveRegEntry (SplitRegister *reg,
				     gboolean do_commit);

/* The xaccSRRedrawReg() method should be called soon 
 *    after the xaccSRSaveRegEntry() method.  It checks the 
 *    change flag for the current entry/split, and if it
 *    has been modified, it causes a redraw of any register
 *    window that could be affected.  That is, it causes 
 *    a redraw of any window showing this split, or any
 *    other split that belongs to this same tansaction. */
void     xaccSRRedrawReg (SplitRegister *reg);

/* The xaccSRHasPendingChanges() method returns TRUE if the register
 *    has changed cells that have not been committed. */
gboolean xaccSRHasPendingChanges (SplitRegister *reg);

/* If TRUE, visually indicate the demarcation between splits with post
 * dates prior to the present, and after. This will only make sense if
 * the splits are ordered primarily by post date. */
void     xaccSRShowPresentDivider (SplitRegister *reg, gboolean show_present);

/* Set the colors used by SplitRegisters */
void     xaccSetSplitRegisterColors (SplitRegisterColors reg_colors);

/* If use_red is TRUE, negative amounts will be printed in red. */
void     xaccSetSplitRegisterColorizeNegative (gboolean use_red);

/* Expand the current transaction if it is collapsed. */
void     xaccSRExpandCurrentTrans (SplitRegister *reg, gboolean expand);

/* Return TRUE if current trans is expanded and style is REG_STYLE_LEDGER. */
gboolean xaccSRCurrentTransExpanded (SplitRegister *reg);

/* Return the debit and credit strings used in the register. */
const char * xaccSRGetDebitString (SplitRegister *reg);
const char * xaccSRGetCreditString (SplitRegister *reg);

/* Private functions, for MultiLedger.c only */
gboolean xaccSRFullRefreshOK (SplitRegister *reg);
void     xaccSRLoadXferCells (SplitRegister *reg, Account *base_account);

CellIOFlags  xaccSRTemplateGetIOFlagsHandler (VirtualLocation virt_loc,
					      gpointer user_data);
gboolean     xaccSRTemplateConfirmHandler (VirtualLocation virt_loc,
					   gpointer user_data);

void gnc_copy_trans_onto_trans(Transaction *from, Transaction *to,
			       gboolean use_cut_semantics,
			       gboolean do_commit);

#endif
