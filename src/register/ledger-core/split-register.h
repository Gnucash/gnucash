/********************************************************************\
 * split-register.h -- split register API                           *
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
 *                                                                  *
\********************************************************************/
/** @addtogroup GUI
 *  @{
 */
/** @addtogroup Register
 *  @{
 */
/** @addtogroup SplitRegister Split Register
 *  @brief GnuCash-specific ledger and journal displays based on
 *  @ref RegisterCore.
 *
 *  @details The split register is a spreadsheet-like area that looks like
 *  a checkbook register. It displays transactions and allows the user to
 *  edit them in-place. The register does @b not contain any of the other
 *  window decorations that one might want to have for a free standing window
 *  (e.g. menubars, *  toolbars, etc.)
 *
 *  The layout of the register is configurable. There's a broad
 *  variety of cell types to choose from: date cells that know
 *  how to parse dates, price cells that know how to parse prices,
 *  etc.  These cells can be laid out in any column; even a multi-row
 *  layout is supported.  The name "split register" is derived from
 *  the fact that this register can display multiple rows of
 *  transaction splits underneath a transaction title/summary row.
 *
 *  An area for entering new transactions is provided at the bottom of
 *  the register.
 *
 *  All user input to the register is handled by the 'cursor', which
 *  is mapped onto one of the displayed rows.
 *
 *  @par Design Notes.
 *  @{
 *  Some notes about the "blank split":@n
 *  Q: What is the "blank split"?@n
 *  A: A new, empty split appended to the bottom of the ledger
 *  window.  The blank split provides an area where the user
 *  can type in new split/transaction info.
 *  The "blank split" is treated in a special way for a number
 *  of reasons:
 *  -  it must always appear as the bottom-most split
 *     in the Ledger window,
 *  -  it must be committed if the user edits it, and
 *     a new blank split must be created.
 *  -  it must be deleted when the ledger window is closed.
 *
 *  To implement the above, the register "user_data" is used
 *  to store an SRInfo structure containing the blank split.
 *  @}
 *
 *  @par Some notes on Commit/Rollback:
 *  @{
 *  There's an engine component and a gui component to the commit/rollback
 *  scheme.  On the engine side, one must always call BeginEdit()
 *  before starting to edit a transaction.  When you think you're done,
 *  you can call CommitEdit() to commit the changes, or RollbackEdit() to
 *  go back to how things were before you started the edit. Think of it as
 *  a one-shot mega-undo for that transaction.
 *
 *  Note that the query engine uses the original values, not the currently
 *  edited values, when performing a sort.  This allows your to e.g. edit
 *  the date without having the transaction hop around in the gui while you
 *  do it.
 *
 *  On the gui side, commits are now performed on a per-transaction basis,
 *  rather than a per-split (per-journal-entry) basis.  This means that
 *  if you have a transaction with a lot of splits in it, you can edit them
 *  all you want without having to commit one before moving to the next.
 *
 * Similarly, the "cancel" button will now undo the changes to all of the
 * lines in the transaction display, not just to one line (one split) at a
 * time.
 * @}
 *
 * @par Some notes on Reloads & Redraws:
 * @{
 * Reloads and redraws tend to be heavyweight. We try to use change flags
 * as much as possible in this code, but imagine the following scenario:
 *
 * Create two bank accounts.  Transfer money from one to the other.
 * Open two registers, showing each account. Change the amount in one window.
 * Note that the other window also redraws, to show the new correct amount.
 *
 * Since you changed an amount value, potentially *all* displayed
 * balances change in *both* register windows (as well as the ledger
 * balance in the main window).  Three or more windows may be involved
 * if you have e.g. windows open for bank, employer, taxes and your
 * entering a paycheck (or correcting a typo in an old paycheck).
 * Changing a date might even cause all entries in all three windows
 * to be re-ordered.
 *
 * The only thing I can think of is a bit stored with every table
 * entry, stating 'this entry has changed since lst time, redraw it'.
 * But that still doesn't avoid the overhead of reloading the table
 * from the engine.
 * @}
 *
 *  The Register itself is independent of GnuCash, and is designed
 *  so that it can be used with other applications.
 *  The Ledger is an adaptation of the Register for use by GnuCash.
 *  The Ledger sets up an explicit visual layout, putting certain
 *  types of cells in specific locations (e.g. date on left, summary
 *  in middle, value at right), and hooks up these cells to
 *  the various GnuCash financial objects.
 *
 *  This code is also theoretically independent of the actual GUI
 *  toolkit/widget-set (it once worked with both Motif and Gnome).
 *  The actual GUI-toolkit specific code is supposed to be in a
 *  GUI portability layer.  Over the years, some gnome-isms may
 *  have snuck in; these should also be cleaned up.
 *
 *  @{
 */
/** @file split-register.h
 *  @brief API for checkbook register display area
 *  @author Copyright (C) 1998-2000 Linas Vepstas <linas@linas.org>
 */
/** @{ */

#ifndef SPLIT_REGISTER_H
#define SPLIT_REGISTER_H

#include <glib.h>

#include "Transaction.h"
#include "table-allgui.h"

/** @brief Register types
 *
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
  RECEIVABLE_REGISTER,
  PAYABLE_REGISTER,
  NUM_SINGLE_REGISTER_TYPES,

  GENERAL_LEDGER = NUM_SINGLE_REGISTER_TYPES,
  INCOME_LEDGER,
  PORTFOLIO_LEDGER,
  SEARCH_LEDGER,

  NUM_REGISTER_TYPES
} SplitRegisterType;

/** Register styles */
typedef enum
{
  REG_STYLE_LEDGER,
  REG_STYLE_AUTO_LEDGER,
  REG_STYLE_JOURNAL
} SplitRegisterStyle;

/** @name Cell Names
 *  T* cells are transaction summary cells
 *  @{
 */
#define ACTN_CELL  "action"
#define BALN_CELL  "balance"
#define CRED_CELL  "credit"
#define DATE_CELL  "date"
#define DDUE_CELL  "date-due"
#define DEBT_CELL  "debit"
#define DESC_CELL  "description"
#define FCRED_CELL "credit-formula"
#define FDEBT_CELL "debit-formula"
#define MEMO_CELL  "memo"
#define MXFRM_CELL "transfer"
#define NOTES_CELL "notes"
#define NUM_CELL   "num"
#define PRIC_CELL  "price"
#define RATE_CELL  "exchrate"
#define RECN_CELL  "reconcile"
#define SHRS_CELL  "shares"
#define TBALN_CELL "trans-balance"
#define TCRED_CELL "trans-credit"
#define TDEBT_CELL "trans-debit"
#define TSHRS_CELL "trans-shares"
#define TYPE_CELL  "split-type"
#define XFRM_CELL  "account"
#define VNOTES_CELL "void-notes"
#define RBALN_CELL "reg-run-balance"
/** @} */

/** @name Cursor Names
  * @{
  */
#define CURSOR_SINGLE_LEDGER  "cursor-single-ledger"
#define CURSOR_DOUBLE_LEDGER  "cursor-double-ledger"
#define CURSOR_SINGLE_JOURNAL "cursor-single-journal"
#define CURSOR_DOUBLE_JOURNAL "cursor-double-journal"
#define CURSOR_SPLIT          "cursor-split"
/** @} */


/** Types of cursors */
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
} SplitRegisterColors;


/** @brief A split register created with ::gnc_split_register_new */
typedef struct split_register SplitRegister;
typedef struct sr_info SRInfo;

/** @brief The type, style and table for the register. */
struct split_register
{
  Table * table;   /**< The table itself that implements the underlying GUI. */

  SplitRegisterType type;
  SplitRegisterStyle style;

  gboolean use_double_line;  /**< whether to use two lines per transaction */
  gboolean is_template;
  gboolean do_auto_complete; /**< whether to use auto-competion */

  SRInfo * sr_info;   /**< private data; outsiders should not access this */
};

/** Callback function type */
typedef gncUIWidget (*SRGetParentCallback) (gpointer user_data);


/* Prototypes ******************************************************/

/** Creates a new split register.
 *
 *  @param type a ::SplitRegisterType to use for the new register
 *
 *  @param style a ::SplitRegisterStyle to use for the new register
 *
 *  @param use_double_line @c TRUE to show two lines for transactions,
 *  @c FALSE for one
 *
 *  @param is_template @c TRUE for a new template, @c FALSE otherwise
 *
 *  @return a newly created ::SplitRegister
 */
SplitRegister * gnc_split_register_new (SplitRegisterType type,
                                        SplitRegisterStyle style,
                                        gboolean use_double_line,
                                        gboolean is_template);

/** Destroys a split register.
 *
 *  @param reg a ::SplitRegister
 */
void gnc_split_register_destroy (SplitRegister *reg);

/** Sets a split register's type, style or line use.
 *
 *  @param reg a ::SplitRegister
 *
 *  @param type a ::SplitRegisterType to use for the register
 *
 *  @param style a ::SplitRegisterStyle to use for the register
 *
 *  @param use_double_line @c TRUE to show two lines for transactions,
 *  @c FALSE for one
 */
void gnc_split_register_config (SplitRegister *reg,
                                SplitRegisterType type,
                                SplitRegisterStyle style,
                                gboolean use_double_line);

/** Sets whether a register uses auto-completion.
 *
 *  @param reg a ::SplitRegister
 *
 *  @param do_auto_complete @c TRUE to use auto-completion, @c FALSE otherwise
 */
void gnc_split_register_set_auto_complete(SplitRegister *reg,
                                          gboolean do_auto_complete);

/** Sets whether a register window is "read only".
 *
 *  @param reg a ::SplitRegister
 *
 *  @param read_only @c TRUE to use "read only" mode, @c FALSE otherwise
 */
void gnc_split_register_set_read_only (SplitRegister *reg, gboolean read_only);


/** Set the template account for use in a template register.
 *
 *  @param reg a ::SplitRegister
 *
 *  @param template_account the account to use for the template
 */
void gnc_split_register_set_template_account (SplitRegister *reg,
                                              Account *template_account);

/** Sets the user data and callback hooks for the register. */
void gnc_split_register_set_data (SplitRegister *reg, gpointer user_data,
                                  SRGetParentCallback get_parent);

/** Returns the class of a register's current cursor.
 *
 *  @param reg a ::SplitRegister
 *
 *  @return the ::CursorClass of the current cursor
 */
CursorClass gnc_split_register_get_current_cursor_class (SplitRegister *reg);

/** Returns the class of the cursor at the given virtual cell location.
 *
 *  @param reg a ::SplitRegister
 *
 *  @param vcell_loc the location of a virtual cell
 *
 *  @return the ::CursorClass of the cursor at @a vcell_loc
 */
CursorClass gnc_split_register_get_cursor_class
                                              (SplitRegister *reg,
                                               VirtualCellLocation vcell_loc);

/** Gets the transaction at the current cursor location, which may be on
 *  the transaction itself or on any of its splits.
 *
 *  @param reg a ::SplitRegister
 *
 *  @return the ::Transaction at the cursor location, or @c NULL
 */
Transaction * gnc_split_register_get_current_trans (SplitRegister *reg);

/** Gets the anchoring split of the transaction at the current cursor location,
 *  which may be on the transaction itself or on any of its splits.
 *
 *  @param reg a ::SplitRegister
 *
 *  @param vcell_loc a pointer to be filled with the location of the
 *  transaction's virtual cell
 *
 *  @return the anchoring ::Split of the transaction
 */
Split *
gnc_split_register_get_current_trans_split (SplitRegister *reg,
                                            VirtualCellLocation *vcell_loc);

/** Returns the split at which the cursor is currently located.
 *
 *  @param reg a ::SplitRegister
 *
 *  @return the ::Split at the cursor location, or the anchoring split
 *  if the cursor is currently on a transaction
 */
Split * gnc_split_register_get_current_split (SplitRegister *reg);

/** Gets the blank split for a register.
 *
 *  @param reg a ::SplitRegister
 *
 *  @return the ::Split used as the blank split, or @c NULL if
 *  there currently isn't one
 */
Split * gnc_split_register_get_blank_split (SplitRegister *reg);

/** Searches the split register for a given split.
 *  The search begins from the bottom row and works backwards. The location
 *  of the first virtual cell that matches will be returned in @a vcell_loc.
 *
 *  @param reg a ::SplitRegister
 *
 *  @param split the ::Split to find
 *
 *  @param vcell_loc a pointer to be filled with the location of the matching
 *  virtual cell
 *
 *  @return @c TRUE if the split was found and the location has been stored
 *  at @a vcell_loc, @c FALSE otherwise
 */
gboolean
gnc_split_register_get_split_virt_loc (SplitRegister *reg, Split *split,
                                       VirtualCellLocation *vcell_loc);

/** Searches the split register for the given split and determines the
 *  location of either its credit (if non-zero) or debit cell.
 *
 *  @param reg a ::SplitRegister
 *
 *  @param split the ::Split to find
 *
 *  @param virt_loc a pointer to be filled with the amount cell's location
 *
 *  @return @c TRUE if the split was found and the location has been stored
 *  at @a virt_loc, @c FALSE otherwise
 */
gboolean
gnc_split_register_get_split_amount_virt_loc (SplitRegister *reg, Split *split,
                                              VirtualLocation *virt_loc);

/** Duplicates either the current transaction or the current split
 *    depending on the register mode and cursor position. Returns the
 *    split just created, or the 'main' split of the transaction just
 *    created, or NULL if nothing happened. */
Split * gnc_split_register_duplicate_current (SplitRegister *reg);

/** Makes a copy of the current entity, either a split or a
 *    transaction, so that it can be pasted later. */
void gnc_split_register_copy_current (SplitRegister *reg);

/** Equivalent to copying the current entity and the deleting it with
 *    the approriate delete method. */
void gnc_split_register_cut_current (SplitRegister *reg);

/** Pastes a previous copied entity onto the current entity, but only
 *    if the copied and current entity have the same type. */
void gnc_split_register_paste_current (SplitRegister *reg);

/** Deletes the split associated with the current cursor, if both are
 *    non-NULL. Deleting the blank split just clears cursor values. */
void gnc_split_register_delete_current_split (SplitRegister *reg);

/** Deletes the transaction associated with the current cursor, if both
 *    are non-NULL. */
void gnc_split_register_delete_current_trans (SplitRegister *reg);

/** Voids the transaction associated with the current cursor, if
 *    non-NULL. */
void gnc_split_register_void_current_trans (SplitRegister *reg,
					    const char *reason);

/** Unvoids the transaction associated with the current cursor, if
 *    non-NULL. */
void gnc_split_register_unvoid_current_trans (SplitRegister *reg);

/** Deletes the non-transaction splits associated wih the current
 *    cursor, if both are non-NULL. */
void gnc_split_register_empty_current_trans_except_split  (SplitRegister *reg, Split *split);
void gnc_split_register_empty_current_trans  (SplitRegister *reg);

/** Cancels any changes made to the current cursor, reloads the cursor
 *    from the engine, reloads the table from the cursor, and updates
 *    the GUI. The change flags are cleared. */
void gnc_split_register_cancel_cursor_split_changes (SplitRegister *reg);

/** Cancels any changes made to the current pending transaction,
 *    reloads the table from the engine, and updates the GUI. The
 *    change flags are cleared. */
void gnc_split_register_cancel_cursor_trans_changes (SplitRegister *reg);

/** Populates the rows of a register.
 *
 *  The rows are filled, based on the register style, with data associated
 *  with the given list of splits @a slist. In addition, an area for the
 *  user to begin entering new transactions is placed at the tail end of the
 *  register. This area is anchored by the "blank split".
 *
 *  The account @a default_account, if provided, is used to determine
 *  various default values for the blank split (such as currency, last check
 *  number, and transfer account) for the blank split.
 *
 *  @param reg a ::SplitRegister
 *
 *  @param slist a list of splits
 *
 *  @param default_account an account to provide defaults for the blank split
 */
void gnc_split_register_load (SplitRegister *reg, GList * slist,
                              Account *default_account);

/** Copy the contents of the current cursor to a split. The split and
 *    transaction that are updated are the ones associated with the
 *    current cursor (register entry) position. If the do_commit flag
 *    is set, the transaction will also be committed. If it is the
 *    blank transaction, and the do_commit flag is set, a refresh will
 *    result in a new blank transaction.  The method returns TRUE if
 *    something was changed. */
gboolean gnc_split_register_save (SplitRegister *reg, gboolean do_commit);

/** Causes a redraw of the register window associated with reg. */
void gnc_split_register_redraw (SplitRegister *reg);

/** Returns TRUE if the register has changed cells. */
gboolean gnc_split_register_changed (SplitRegister *reg);

/** If TRUE, visually indicate the demarcation between splits with post
 * dates prior to the present, and after. This will only make sense if
 * the splits are ordered primarily by post date. */
void gnc_split_register_show_present_divider (SplitRegister *reg,
                                              gboolean show_present);

/** Expand the current transaction if it is collapsed. */
void gnc_split_register_expand_current_trans (SplitRegister *reg,
                                              gboolean expand);

/** Mark the current transaction as collapsed, and do callbacks. */
void gnc_split_register_collapse_current_trans (SplitRegister *reg);

/** Return TRUE if current trans is expanded and style is REG_STYLE_LEDGER. */
gboolean gnc_split_register_current_trans_expanded (SplitRegister *reg);

/** Return the debit string used in the register. */
const char * gnc_split_register_get_debit_string (SplitRegister *reg);

/** Return the credit string used in the register. */
const char * gnc_split_register_get_credit_string (SplitRegister *reg);


/** Pop up the exchange-rate dialog, maybe, for the current split.
 * If force_dialog is TRUE, the forces the dialog to to be called.
 * If the dialog does not complete successfully, then return TRUE.
 * Return FALSE in all other cases (meaning "move on")
 */
gboolean
gnc_split_register_handle_exchange (SplitRegister *reg, gboolean force_dialog);

/* returns TRUE if begin_edit was aborted */
gboolean
gnc_split_register_begin_edit_or_warn(SRInfo *info, Transaction *trans);

/** @} */
/** @} */
/** @} */
/** @} */
/* -------------------------------------------------------------- */

/** Private function -- outsiders must not use this */
gboolean gnc_split_register_full_refresh_ok (SplitRegister *reg);

/** Private function -- outsiders must not use this */
void gnc_copy_trans_onto_trans (Transaction *from, Transaction *to,
                                gboolean use_cut_semantics,
                                gboolean do_commit);

#endif
