/********************************************************************\
 * messages_i18n.h -- internationalized messages for GnuCash        *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1997, 1998, 1999,2000 Linas Vepstas                *
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
 *   Author: Rob Clark                                              *
 * Internet: rclark@cs.hmc.edu                                      *
 *  Address: 609 8th Street                                         *
 *           Huntington Beach, CA 92648-4632                        *
\********************************************************************/

#ifndef __XACC_MESSAGES_I18N_H__
#define __XACC_MESSAGES_I18N_H__

#if defined(HAVE_GETTEXT)             /* HAVE_GETTEXT */

#include <libintl.h>
#include <locale.h>

#undef _

#ifdef DISABLE_GETTEXT_UNDERSCORE
#define _(String) (String)
#else                                 /* ENABLE_GETTEXT_UNDERSCORE */
#define _(String) gettext(String)
#endif		                      /* End ENABLE_GETTEXT_UNDERSCORE */

#else                                 /* Not HAVE_GETTEXT */

#define _(String)       (String)
#define gettext(String) (String)

#endif                                /* End Not HAVE_GETTEXT */

#undef  N_
#define N_(String) (String)


/** MISC INTERNATIONALIZATION PIECES-PARTS: ******************************/

/* This will be used if localeconv cannot find a value. */
#define CURRENCY_SYMBOL  _("$")


/** DIALOG BOX MESSAGES: ********************************************/
#define ABOUT_MSG         _("The GnuCash personal finance manager.\n"\
                            "The GNU way to manage your money!")
#define ACC_ADJUST_MSG    _("To adjust an account's balance, you must "\
                            "first\nchoose an account to adjust.\n")
#define ACC_BAD_PARENT_MSG _("You must choose a valid parent account.")
#define ACC_NEW_MSG       _("Do you want to create a new account?\n"\
                            "If not, then please select an account\n"\
                            "to open in the main window.\n")
#define ACC_EDIT_MSG      _("To edit an account, you must first\n"\
                            "choose an account to edit.\n")
#define ACC_DEL_MSG       _("To delete an account, you must first\n"\
                            "choose an account to delete.\n")
#define ACC_DEL_SURE_MSG  _("Are you sure you want to delete the %s account?")
#define ACC_NO_NAME_MSG   _("The account must be given a name! \n")
#define ACC_OPEN_MSG      _("To open an account, you must first\n"\
                            "choose an account to open.\n")
#define ACC_SCRUB_MSG     _("You must select an account to scrub.")
#define ACC_TYPE_MSG      _("You must select an account type.")
#define ACC_RECONCILE_MSG _("To reconcile an account, you must first\n"\
                            "choose an account to reconcile.\n")
#define AMOUNT_NUM_MSG    _("The amount must be a number.")
#define BALANCE_NUM_MSG   _("The balance must be a number.")
#define CHANGE_RECN_MSG   _("Do you really want to mark this transaction "\
                            "not reconciled?\nDoing so might make future "\
                            "reconciliation difficult!")
#define DEL_SPLITS_MSG    _("Delete all the splits")
#define DEL_TRANS_MSG     _("Delete the whole transaction")
#define DEL_USUAL_MSG     _("This selection will delete the whole "\
                            "transaction. This is what you usually want.")
#define DEL_WARN_MSG      _("Warning: Just deleting all the splits will "\
                            "make your account unbalanced. You probably "\
                            "shouldn't do this unless you're going to "\
                            "immediately add another split to bring the "\
                            "transaction back into balance.")
#define EDIT_CURRENCY_MSG _("It is dangerous to change the currency type\n"\
                            "of an account. You asked to change it\n"\
                            "from %s to %s.\nAre You Sure?\n") 
#define EDIT_SECURITY_MSG _("It is dangerous to change the security\n"\
                            "of an account. You asked to change it\n"\
                            "from %s to %s.\nAre You Sure?\n") 
#define FILE_TOO_OLD_MSG  _("This file is from an older version of "\
                            "GnuCash.  \nDo you want to continue?")
#define FILE_TOO_NEW_MSG  _("This file appears to be from a newer version "\
                            "of GnuCash. You must upgrade GnuCash to read "\
                            "this file.")
#define FILE_BAD_READ_MSG _("There was an error reading the file. \n"\
                            "Do you want to continue?")
#define FILE_EWRITE_MSG   _("There was an error writing the file\n     %s" \
                            "\n\n%s")
#define FILE_EOPEN_MSG    _("There was an error opening the file\n     %s" \
                            "\n\n%s")
#define FILE_ECLOSE_MSG   _("There was an error closing the file\n     %s" \
                            "\n\n%s")
#define FILE_NOT_FOUND_MSG _("The file \n    %s\n could not be found.")
#define FILE_EMPTY_MSG    _("The file \n    %s\n is empty.")
#define FMB_SAVE_MSG      _("Changes have been made since the last " \
                            "Save. Save the data to file?")
#define FMB_EEXIST_MSG    _("The file \n    %s\n already exists.\n" \
                            "Are you sure you want to overwrite it?")
#define FMB_INVALID_MSG   _("The filepath \n    %s\n" \
                            "is not a valid location in the filesystem.")
#define FMB_LOCKED_MSG    _("The file \n    %s\n" \
                            "appears to be in use by another user.\n" \
                            "If this is not right, remove the .LCK file " \
                            "and try again.")
#define GNOME_PRINT_MSG   _("You need to install the gnome-print library.")
#define QIF_LOAD_FAILED_FORMAT_MSG _("QIF file load failed:\n%s")
#define QIF_LOAD_WARNING_FORMAT_MSG _("QIF file load warning:\n%s")
#define QIF_PARSE_FAILED_FORMAT_MSG _("QIF file parse failed:\n%s")
#define QIF_PARSE_WARNING_FORMAT_MSG _("QIF file parse warning:\n%s")

#define QUOTE_SRC_MSG     _("The source for price quotes")
#define RECN_BALN_WARN    _("The account is not balanced.\n" \
                            "Are you sure you want to finish?")
#define RECN_CANCEL_WARN  _("You have made changes to this reconcile " \
                            "window.\nAre you sure you want to cancel?")
#define RECN_TRANS_WARN   _("Warning! This is a reconciled transaction. " \
                            "Do you want do continue?")
#define REG_CURR_MSG      _("You cannot transfer funds from the %s " \
                            "account.\nIt does not have a matching " \
                            "currency.")
#define REPORT_ERR_MSG    _("Error executing scheme report.")
#define REPORT_NOPARM_MSG _("This report has no parameters.")
#define SHOW_CAT_MSG      _("Show the income and expense accounts.")
#define TRANS_CHANGED_MSG _("The current transaction has been changed.\n"\
                            "Would you like to record it?")
#define TRANS_DEL_MSG     _("Are you sure you want to delete\n   %s\n"\
                            "from the transaction\n   %s ?")
#define TRANS_DEL2_MSG    _("Are you sure you want to delete the current "\
                            "transaction?")
#define TRANS_RECALC_TITLE _("Recalculate Transaction") 
#define TRANS_RECALC_MSG  _("The values entered for this transaction "\
                            "are inconsistent.\nWhich value would you "\
                            "like to have recalculated?\n")
#define TYPE_WARN1_MSG    _("The types of all the parent accounts and their "\
                            "subaccounts must be changed to %s.\nIs that "\
                            "what you want to do?")
#define TYPE_WARN2_MSG    _("The types of all the subaccounts must be "\
                            "changed to %s.\nIs that what you want to do?")
#define VERIFY_CHANGE_MSG _("The following changes must be made. Continue?")
#define XFER_NSF_MSG      _("There must be at least two accounts\n"\
                            "created before you can transfer funds.")
#define XFER_CURR_MSG     _("You cannot transfer between those accounts.\n" \
                            "They do not have a common currency.")
#define XFER_DIFF_MSG     _("The \"From\" and \"To\" accounts\n must be " \
                            "different!")
#define XFER_SAME_MSG     _("You can't transfer from and to the same " \
                            "account!")
#define XFER_NO_ACC_MSG   _("You must specify an account to transfer from,\n"\
                            "or to, or both, for this transaction.\n" \
                            "Otherwise, it will not be recorded.")


/* Tooltip phrases */
#define TOOLTIP_ADJUST_N       N_("Adjust the balance of the selected " \
                                  "account")
#define TOOLTIP_ADJUST          _(TOOLTIP_ADJUST_N)
#define TOOLTIP_ADJUST_AMOUNT   _("Enter the new balance")
#define TOOLTIP_ADJUST_DATE     _("Enter the date you want the balance "\
                                  "adjusted")
#define TOOLTIP_ADJUST_END_N   N_("Adjust the ending balance")
#define TOOLTIP_ADJUST_END      _(TOOLTIP_ADJUST_END_N)
#define TOOLTIP_ADJUST_REG_N   N_("Adjust the balance of the main account "\
                                  "for this register")
#define TOOLTIP_ADJUST_REG      _(TOOLTIP_ADJUST_REG_N)
#define TOOLTIP_AUTO_DOUBLE_N  N_("Double line mode with a multi-line cursor")
#define TOOLTIP_AUTO_DOUBLE     _(TOOLTIP_AUTO_DOUBLE_N)
#define TOOLTIP_AUTO_SINGLE_N  N_("Single line mode with a multi-line cursor")
#define TOOLTIP_AUTO_SINGLE     _(TOOLTIP_AUTO_SINGLE_N)
#define TOOLTIP_BLANK_TRANS_N  N_("Move to the blank transaction at the "\
                                  "bottom of the register")
#define TOOLTIP_BLANK_TRANS     _(TOOLTIP_BLANK_TRANS_N)
#define TOOLTIP_CANCEL_TRANS_N N_("Cancel the current transaction")
#define TOOLTIP_CANCEL_TRANS    _(TOOLTIP_CANCEL_TRANS_N)
#define TOOLTIP_CLOSE_HTML      _("Close this HTML window")
#define TOOLTIP_CLOSE_REG_N    N_("Close this register window")
#define TOOLTIP_CLOSE_REG       _(TOOLTIP_CLOSE_REG_N)
#define TOOLTIP_COPY_TRANS_N   N_("Copy the selected transaction")
#define TOOLTIP_COPY_TRANS      _(TOOLTIP_COPY_TRANS_N)
#define TOOLTIP_CURRENCY        _("Enter a 3-letter ISO currency code such " \
                                  "as USD (US Dollars)")
#define TOOLTIP_CUT_TRANS_N    N_("Cut the selected transaction")
#define TOOLTIP_CUT_TRANS       _(TOOLTIP_CUT_TRANS_N)
#define TOOLTIP_DATE_RANGE_N   N_("Set the date range of this register")
#define TOOLTIP_DATE_RANGE      _(TOOLTIP_DATE_RANGE_N)
#define TOOLTIP_DELETE_N       N_("Delete selected account")
#define TOOLTIP_DELETE	        _(TOOLTIP_DELETE_N)
#define TOOLTIP_DEL_TRANS_N    N_("Delete the current transaction")
#define TOOLTIP_DEL_TRANS       _(TOOLTIP_DEL_TRANS_N)
#define TOOLTIP_DOUBLE_LINE_N  N_("Show transactions on two lines with "\
                                 "more information")
#define TOOLTIP_DOUBLE_LINE     _(TOOLTIP_DOUBLE_LINE_N)
#define TOOLTIP_DUP_TRANS_N    N_("Make a copy of the current transaction")
#define TOOLTIP_DUP_TRANS       _(TOOLTIP_DUP_TRANS_N)
#define TOOLTIP_EDIT_N         N_("Edit the selected account")
#define TOOLTIP_EDIT	        _(TOOLTIP_EDIT_N)
#define TOOLTIP_EDIT_REG_N     N_("Edit the main account for this register")
#define TOOLTIP_EDIT_REG        _(TOOLTIP_EDIT_REG_N)
#define TOOLTIP_EDIT_TRANS_N   N_("Edit the current transaction")
#define TOOLTIP_EDIT_TRANS      _(TOOLTIP_EDIT_TRANS_N)
#define TOOLTIP_EXIT_N         N_("Exit GnuCash")
#define TOOLTIP_EXIT	        _(TOOLTIP_EXIT_N)
#define TOOLTIP_EXPORT_REPORT   _("Export HTML-formatted report to file")
#define TOOLTIP_FIND_N         N_("Find transactions with a search.")
#define TOOLTIP_FIND            _(TOOLTIP_FIND_N)
#define TOOLTIP_HELP_N         N_("Open the GnuCash help window")
#define TOOLTIP_HELP            _(TOOLTIP_HELP_N)
#define TOOLTIP_HTML_BACK       _("Move back one step in the history")
#define TOOLTIP_HTML_FORW       _("Move forward one step in the history")
#define TOOLTIP_IMPORT_QIF_N   N_("Import a Quicken QIF file")
#define TOOLTIP_IMPORT_QIF      _(TOOLTIP_IMPORT_QIF_N)
#define TOOLTIP_JUMP_TRANS_N   N_("Jump to the corresponding transaction in "\
                                  "the other account")
#define TOOLTIP_JUMP_TRANS      _(TOOLTIP_JUMP_TRANS_N)
#define TOOLTIP_MAN_N          N_("Open the GnuCash Manual")
#define TOOLTIP_MAN             _(TOOLTIP_MAN_N)
#define TOOLTIP_MULTI_LINE_N   N_("Show transactions on multiple lines with "\
                                  "one line for each split")
#define TOOLTIP_MULTI_LINE      _(TOOLTIP_MULTI_LINE_N)
#define TOOLTIP_MULTI_SPLIT     _("This transaction has multiple splits; "\
                                  "switch to multi-line mode to see them all")
#define TOOLTIP_NEW_N	       N_("Create a new account")
#define TOOLTIP_NEW	        _(TOOLTIP_NEW_N)
#define TOOLTIP_NEW_FILE_N     N_("Create a new file")
#define TOOLTIP_NEW_FILE        _(TOOLTIP_NEW_FILE_N)
#define TOOLTIP_NEW_TRANS_N    N_("Add a new transaction to the account")
#define TOOLTIP_NEW_TRANS       _(TOOLTIP_NEW_TRANS_N)
#define TOOLTIP_OPEN_N         N_("Open the selected account")
#define TOOLTIP_OPEN	        _(TOOLTIP_OPEN_N)
#define TOOLTIP_OPEN_ACC_N     N_("Open the account")
#define TOOLTIP_OPEN_ACC        _(TOOLTIP_OPEN_ACC_N)
#define TOOLTIP_OPEN_SUB_N     N_("Open the selected account and all its "\
                                  "subaccounts")
#define TOOLTIP_OPEN_SUB        _(TOOLTIP_OPEN_SUB_N)
#define TOOLTIP_OPEN_FILE_N    N_("Open a file")
#define TOOLTIP_OPEN_FILE       _(TOOLTIP_OPEN_FILE_N)
#define TOOLTIP_PASTE_TRANS_N  N_("Paste the transaction clipboard")
#define TOOLTIP_PASTE_TRANS     _(TOOLTIP_PASTE_TRANS_N)
#define TOOLTIP_PREFERENCES_N  N_("Open the global preferences dialog")
#define TOOLTIP_PREFERENCES     _(TOOLTIP_PREFERENCES_N)
#define TOOLTIP_PRINT_CHECK_N  N_("Print a check using a standard format")
#define TOOLTIP_PRINT_CHECK     _(TOOLTIP_PRINT_CHECK_N)
#define TOOLTIP_RECONCILE_N    N_("Reconcile the selected account")
#define TOOLTIP_RECONCILE       _(TOOLTIP_RECONCILE_N)
#define TOOLTIP_RECN_CANCEL_N  N_("Cancel the reconciliation of this account")
#define TOOLTIP_RECN_CANCEL     _(TOOLTIP_RECN_CANCEL_N)
#define TOOLTIP_RECN_FINISH_N  N_("Finish the reconciliation of this account")
#define TOOLTIP_RECN_FINISH     _(TOOLTIP_RECN_FINISH_N)
#define TOOLTIP_RECN_INFO_N    N_("Change the reconcile information "\
                                  "including statement date and ending "\
                                  "balance.")
#define TOOLTIP_RECN_REG_N     N_("Reconcile the main account for this "\
                                  "register")
#define TOOLTIP_RECN_REG        _(TOOLTIP_RECN_REG_N)
#define TOOLTIP_RECORD_N       N_("Record the current transaction")
#define TOOLTIP_RECORD          _(TOOLTIP_RECORD_N)
#define TOOLTIP_REPORT_PARM     _("Set the parameters for this report")
#define TOOLTIP_SAVE_FILE_N    N_("Save the file to disk")
#define TOOLTIP_SAVE_FILE       _(TOOLTIP_SAVE_FILE_N)
#define TOOLTIP_SCRUB_ACCT_N   N_("Identify and fix problems in the account")
#define TOOLTIP_SCRUB_ACCT      _(TOOLTIP_SCRUB_ACCT_N)
#define TOOLTIP_SCRUB_ALL_N    N_("Identify and fix problems in all the "\
                                  "accounts")
#define TOOLTIP_SCRUB_ALL       _(TOOLTIP_SCRUB_ALL_N)
#define TOOLTIP_SCRUB_SUB_N    N_("Identify and fix problems in the account "\
                                  "and its subaccounts")
#define TOOLTIP_SCRUB_SUB       _(TOOLTIP_SCRUB_SUB_N)
#define TOOLTIP_SCRUB_REG_N    N_("Identify and fix problems in the "\
                                  "accounts of this register")
#define TOOLTIP_SCRUB_REG       _(TOOLTIP_SCRUB_REG_N)
#define TOOLTIP_SET_DEFAULT     _("Set the option to its default value")
#define TOOLTIP_SHOW_ALL_N     N_("Show all of the transactions in the "\
                                  "account")
#define TOOLTIP_SHOW_ALL        _(TOOLTIP_SHOW_ALL_N)
#define TOOLTIP_SINGLE_LINE_N  N_("Show transactions on single lines")
#define TOOLTIP_SINGLE_LINE     _(TOOLTIP_SINGLE_LINE_N)
#define TOOLTIP_SORT_BY_AMNT_N N_("Sort by Amount")
#define TOOLTIP_SORT_BY_AMNT    _(TOOLTIP_SORT_BY_AMNT_N)
#define TOOLTIP_SORT_BY_DATE_N N_("Sort by Date")
#define TOOLTIP_SORT_BY_DATE    _(TOOLTIP_SORT_BY_DATE_N)
#define TOOLTIP_SORT_BY_DESC_N N_("Sort by Description")
#define TOOLTIP_SORT_BY_DESC    _(TOOLTIP_SORT_BY_DESC_N)
#define TOOLTIP_SORT_BY_ENTERED_N N_("Sort by the date of entry")
#define TOOLTIP_SORT_BY_ENTERED    _(TOOLTIP_SORT_BY_ENTERED_N)
#define TOOLTIP_SORT_BY_MEMO_N N_("Sort by Memo")
#define TOOLTIP_SORT_BY_MEMO    _(TOOLTIP_SORT_BY_MEMO_N)
#define TOOLTIP_SORT_BY_NUM_N  N_("Sort by Num")
#define TOOLTIP_SORT_BY_NUM     _(TOOLTIP_SORT_BY_NUM_N)
#define TOOLTIP_SORT_BY_STMT_N N_("Sort by the statement date "\
                                  "(unreconciled items last")
#define TOOLTIP_SORT_BY_STMT    _(TOOLTIP_SORT_BY_STMT_N)
#define TOOLTIP_STANDARD_ORD_N N_("Keep normal account order")
#define TOOLTIP_STANDARD_ORD    _(TOOLTIP_STANDARD_ORD_N)
#define TOOLTIP_TRANSFER_N     N_("Transfer funds from one account to "\
                                  "another")
#define TOOLTIP_TRANSFER        _(TOOLTIP_TRANSFER_N)


/* Register cell help phrases */
#define ACTION_CELL_HELP  _("Enter the type of transaction, or choose "\
                            "one from the list")
#define DESC_CELL_HELP    _("Enter a description of the transaction")
#define MEMO_CELL_HELP    _("Enter a description of the split")
#define NUM_CELL_HELP     _("Enter the transaction number, such as the "\
                            "check number")
#define PRICE_CELL_HELP   _("Enter the share price")
#define SELL_CELL_HELP    _("Enter the number of shares sold")
#define VALUE_CELL_HELP   _("Enter the total value of the shares")
#define XFER_CELL_HELP    _("Enter the account to transfer from, or choose "\
                            "one from the list")
#define XFER_TO_CELL_HELP _("Enter the account to transfer to, or choose "\
                            "one from the list")


/* Menu strings with underscore accelerators */
#define ACCOUNT_MENU_STR_N       N_("_Account")
#define ACCOUNT_MENU_STR          _(ACCOUNT_MENU_STR_N)
#define ACCOUNTS_MENU_STR_N      N_("_Accounts")
#define ACCOUNTS_MENU_STR         _(ACCOUNTS_MENU_STR_N)
#define ADJ_BALN_MENU_STR         _("_Adjust Balance")
#define ADJ_BALN_MENU_E_STR_N    N_("_Adjust Balance...")
#define BLANK_MENU_STR_N         N_("_Blank")
#define BLANK_MENU_STR            _(BLANK_MENU_STR_N)
#define CANCEL_MENU_STR_N        N_("_Cancel")
#define CANCEL_MENU_STR           _(CANCEL_MENU_STR_N)
#define COPY_TRANS_STR_N         N_("Copy Transaction")
#define COPY_TRANS_STR            _(COPY_TRANS_STR_N)
#define CUT_TRANS_STR_N          N_("Cut Transaction")
#define CUT_TRANS_STR             _(CUT_TRANS_STR_N)
#define DATE_RANGE_MENU_STR_N    N_("_Date Range")
#define DATE_RANGE_MENU_STR       _(DATE_RANGE_MENU_STR_N)
#define DEL_ACC_MENU_STR_N       N_("_Delete Account")
#define DEL_ACC_MENU_STR          _(DEL_ACC_MENU_STR_N)
#define DELETE_MENU_STR_N        N_("_Delete")
#define DELETE_MENU_STR           _(DELETE_MENU_STR_N)
#define DUPLICATE_MENU_STR_N     N_("D_uplicate")
#define DUPLICATE_MENU_STR        _(DUPLICATE_MENU_STR_N)
#define EDIT_MENU_STR_N          N_("_Edit")
#define EDIT_MENU_STR             _(EDIT_MENU_STR_N)
#define EDIT_ACC_MENU_STR_N      N_("_Edit Account")
#define EDIT_ACC_MENU_STR         _(EDIT_ACC_MENU_STR_N)
#define END_BALN_MENU_STR         _("_Ending Balance")
#define END_BALN_MENU_E_STR_N    N_("_Ending Balance...")
#define FINISH_MENU_STR_N        N_("_Finish")
#define FINISH_MENU_STR           _(FINISH_MENU_STR_N)
#define HELP_MENU_STR_N          N_("_Help")
#define HELP_MENU_STR             _(HELP_MENU_STR_N)
#define JUMP_MENU_STR_N          N_("_Jump")
#define JUMP_MENU_STR             _(JUMP_MENU_STR_N)
#define MAN_MENU_STR_N           N_("_Manual")
#define MAN_MENU_STR              _(MAN_MENU_STR_N)
#define NEW_MENU_STR_N           N_("_New")
#define NEW_MENU_STR              _(NEW_MENU_STR_N)
#define NEW_ACC_MENU_STR          _("_New Account")
#define NEW_ACC_MENU_E_STR_N     N_("_New Account...")
#define OPEN_ACC_MENU_STR_N      N_("_Open Account")
#define OPEN_ACC_MENU_STR         _(OPEN_ACC_MENU_STR_N)
#define OPEN_SUB_MENU_STR_N      N_("Open S_ubaccounts")
#define OPEN_SUB_MENU_STR         _(OPEN_SUB_MENU_STR_N)
#define PASTE_TRANS_STR_N        N_("Paste Transaction")
#define PASTE_TRANS_STR           _(PASTE_TRANS_STR_N)
#define PREFERENCES_MENU_STR      _("_Preferences")
#define PREFERENCES_MENU_E_STR_N N_("_Preferences...")
#define PRINT_CHECK_MENU_E_STR_N N_("_Print Check... (unfinished!)")
#define RECN_INFO_MENU_E_STR_N   N_("_Reconcile Information...")
#define RECONCILE_MENU_E_STR_N   N_("_Reconcile...")
#define RECONCILE_MENU_STR_N     N_("_Reconcile")
#define RECONCILE_MENU_STR        _(RECONCILE_MENU_STR_N)
#define RECORD_MENU_STR_N        N_("_Record")
#define RECORD_MENU_STR           _(RECORD_MENU_STR_N)
#define REGISTER_MENU_STR_N      N_("_Register")
#define REGISTER_MENU_STR         _(REGISTER_MENU_STR_N)
#define SCRUB_MENU_STR_N         N_("_Scrub")
#define SCRUB_MENU_STR            _(SCRUB_MENU_STR_N)
#define SCRUB_ACC_MENU_STR_N     N_("Scrub A_ccount")
#define SCRUB_ACC_MENU_STR        _(SCRUB_ACC_MENU_STR_N)
#define SCRUB_ALL_MENU_STR_N     N_("Scrub A_ll")
#define SCRUB_ALL_MENU_STR        _(SCRUB_ALL_MENU_STR_N)
#define SCRUB_SUB_MENU_STR_N     N_("Scrub Su_baccounts")
#define SCRUB_SUB_MENU_STR        _(SCRUB_SUB_MENU_STR_N)
#define SET_RANGE_MENU_E_STR_N   N_("Set _Range...")
#define SET_RANGE_MENU_STR        _("Set _Range")
#define SHOW_ALL_MENU_STR_N      N_("Show _All")
#define SHOW_ALL_MENU_STR         _(SHOW_ALL_MENU_STR_N)
#define SORT_ORDER_MENU_STR_N    N_("Sort _Order")
#define SORT_ORDER_MENU_STR       _(SORT_ORDER_MENU_STR_N)
#define STYLE_MENU_STR_N         N_("_Style")
#define STYLE_MENU_STR            _(STYLE_MENU_STR_N)
#define TRANSACTION_MENU_STR_N   N_("_Transaction")
#define TRANSACTION_MENU_STR      _(TRANSACTION_MENU_STR_N)
#define TRANSFER_MENU_STR         _("_Transfer")
#define TRANSFER_MENU_E_STR_N    N_("_Transfer...")


/** MISC INTERNATIONALIZATION STRINGS: ******************************/

/* phrases */
#define ACC_CODE_STR        _("Account Code")
#define ACC_NAME_STR        _("Account Name")
#define ACC_TYPE_STR        _("Account Type")
#define ADJ_BALN_STR        _("Adjust Balance")
#define AUTO_DOUBLE_STR_N  N_("Auto Double")
#define AUTO_DOUBLE_STR     _(AUTO_DOUBLE_STR_N)
#define AUTO_SINGLE_STR_N  N_("Auto Single")
#define AUTO_SINGLE_STR     _(AUTO_SINGLE_STR_N)
#define CLEAR_ALL_STR       _("Clear All")
#define CLOSE_WIN_STR       _("Close Window")
#define CREDIT_CARD_STR     _("Credit Card")
#define CREDITLINE_STR      _("Credit Line")         /* Line of Credit */
#define DATE_RANGE_STR      _("Date Range")
#define DEL_ACC_STR         _("Delete Account")
#define DEL_TRANS_STR       _("Delete Transaction")
#define DOUBLE_LINE_STR_N  N_("Double Line")
#define DOUBLE_LINE_STR     _(DOUBLE_LINE_STR_N)
#define EDIT_ACCT_STR       _("Edit Account")
#define END_BALN_STR        _("Ending Balance")
#define END_DATE_STR        _("End date")
#define EXPORT_TO_STR       _("Export To")
#define FINISH_STR          _("Finish")
#define FROM_NOW_STR        _("From Now")
#define GENERAL_LEDGER_STR  _("General Ledger")
#define HIDE_INC_EXP_STR    _("Hide Inc/Exp")
#define IMPORT_QIF_E_STR_N N_("Import QIF...")
#define IMPORT_QIF_STR      _("Import QIF")
#define LOST_ACC_STR        _("Lost Accounts")
#define MONEYMRKT_STR       _("Money Market")
#define MULTI_LINE_STR_N   N_("Multi Line")
#define MULTI_LINE_STR      _(MULTI_LINE_STR_N)
#define MUTUAL_FUND_STR     _("Mutual Fund")
#define NEW_ACC_STR         _("New Account")
#define NEW_BALN_STR        _("New Balance")
#define NEW_FILE_STR_N     N_("New File")
#define NEW_FILE_STR        _(NEW_FILE_STR_N)
#define NEW_TOP_ACCT_STR    _("New top level account")
#define NEW_VALUE_STR       _("New Value")
#define NO_DESC_STR         _("No description")
#define OLD_VALUE_STR       _("Old Value")
#define OPEN_ACC_STR        _("Open Account")
#define OPEN_BALN_STR       _("Opening Balance")
#define OPEN_FILE_STR       _("Open File")
#define OPEN_SUB_STR        _("Open Subaccounts")
#define PICK_ONE_STR        _("Pick One")
#define PARENT_ACC_STR      _("Parent Account")
#define PREV_BALN_STR       _("Previous Balance")
#define PURCH_PRIC_STR      _("Purch Price")
#define QUOTE_SRC_STR       _("Price Quote Source")
#define RECONCILE_BALN_STR  _("Reconciled Balance")
#define RECONCILE_INFO_STR  _("Reconcile Information")
#define REG_DATE_RANGES_STR _("Register date ranges")
#define SALE_PRIC_STR       _("Sale Price")
#define SAVE_AS_STR         _("Save As")
#define SCRUB_ACCT_STR      _("Scrub Account")
#define SCRUB_ALL_STR       _("Scrub All")
#define SCRUB_SUBACCTS_STR  _("Scrub Subaccounts")
#define SELECT_ALL_STR      _("Select All")
#define SELECT_DEFAULT_STR  _("Select Default")
#define SET_DATE_RANGE_STR  _("Set Date Range")
#define SET_RANGE_STR       _("Set Range")
#define SET_TO_DEFAULT_STR  _("Set to default")
#define SETUP_ACCT_STR      _("Set Up Account")
#define SHOW_ALL_STR        _("Show All")
#define SHOW_INC_EXP_STR    _("Show Inc/Exp")
#define SHOW_CATEGORIES_STR _("Show Categories")
#define SHOW_EARLIEST_STR   _("Show Earliest")
#define SHOW_LATEST_STR     _("Show Latest")
#define SINGLE_LINE_STR_N  N_("Single Line")
#define SINGLE_LINE_STR     _(SINGLE_LINE_STR_N)
#define SORT_BY_AMNT_STR_N N_("Sort by Amount")
#define SORT_BY_AMNT_STR    _(SORT_BY_AMNT_STR_N)
#define SORT_BY_DATE_STR_N N_("Sort by Date")
#define SORT_BY_DATE_STR    _(SORT_BY_DATE_STR_N)
#define SORT_BY_DESC_STR_N N_("Sort by Description")
#define SORT_BY_DESC_STR    _(SORT_BY_DESC_STR_N)
#define SORT_BY_ENTERED_STR_N N_("Sort by date of entry")
#define SORT_BY_ENTERED_STR    _(SORT_BY_ENTERED_STR_N)
#define SORT_BY_MEMO_STR_N N_("Sort by Memo")
#define SORT_BY_MEMO_STR    _(SORT_BY_MEMO_STR_N)
#define SORT_BY_NUM_STR_N  N_("Sort by Num")
#define SORT_BY_NUM_STR     _(SORT_BY_NUM_STR_N)
#define SORT_BY_STMT_STR_N N_("Sort by statement date")
#define SORT_BY_STMT_STR    _(SORT_BY_STMT_STR_N)
#define SORT_ORDER_STR      _("Sort Order")
#define START_DATE_STR      _("Start date")
#define START_BALN_STR      _("Starting Balance")
#define STANDARD_ORDER_STR_N N_("Standard order")
#define STANDARD_ORDER_STR    _(STANDARD_ORDER_STR_N)
#define STATEMENT_DATE_C_STR _("Statement Date:")
#define TOP_ACCT_STR        _("Top level account")
#define TOTAL_SHARES_STR    _("Total Shares")
#define VERIFY_CHANGES_STR  _("Verify Changes")
#define XFER_INFO           _("Transfer Information")
#define XFER_MONEY_STR      _("Transfer Money")
#define XFRM_STR            _("Transfer From")
#define XFTO_STR            _("Transfer To")

/* single words */
#define ABOUT_STR           _("About")
#define ACCOUNT_STR         _("Account")
#define ACCOUNTS_STR        _("Accounts")
#define ACC_INFO_STR        _("Account Information")
#define ACH_STR             _("AutoDep")   /* Automatic Check Handling auto deposit */
#define ACTION_STR          _("Action")
#define ACTIVITIES_STR      _("Activities")
#define AGO_STR             _("Ago")
#define AMT_STR             _("Amount")
#define APPR_STR            _("Appreciation")
#define ARU_STR             _("Phone") /* Automated Response Unit telephone xfer */
#define ASSET_STR           _("Asset")
#define ASSETS_STR          _("Assets")
#define ATM_STR             _("ATM")          /* automatic teller machine */
#define BACK_STR            _("Back")
#define BALN_STR            _("Balance")
#define BALN_EURO_STR       _("Balance")
#define BANK_STR            _("Bank")
#define BLANK_STR_N        N_("Blank")
#define BLANK_STR           _(BLANK_STR_N)
#define BOUGHT_STR          _("Bought")
#define BUY_STR             _("Buy")
#define CANCEL_STR_N       N_("Cancel")
#define CANCEL_STR          _(CANCEL_STR_N)
#define CASH_STR            _("Cash")
#define CHANGED_STR         _("Changed")
#define CHARGE_STR          _("Charge")
#define CHECK_STR           _("Check")
#define CHECKING_STR        _("Checking")
#define CLEARED_STR         _("Cleared")
#define CLOSE_STR_N        N_("Close")
#define CLOSE_STR           _(CLOSE_STR_N)
#define COMMIT_STR          _("Commit")
#define CREATE_STR          _("Create")
#define CREDIT_STR          _("Credit")
#define CREDITS_STR_N      N_("Credits")
#define CREDITS_STR         _(CREDITS_STR_N)
#define CURRENCY_STR        _("Currency")
#define DATE_STR            _("Date")
#define DAYS_STR            _("Days")
#define DEBIT_STR           _("Debit")
#define DEBITS_STR_N       N_("Debits")
#define DEBITS_STR          _(DEBITS_STR_N)
#define DECREASE_STR        _("Decrease")
#define DEFICIT_STR         _("Deficit")
#define DELETE_STR_N       N_("Delete")
#define DELETE_STR          _(DELETE_STR_N)
#define DEPOSIT_STR         _("Deposit")
#define DEPR_STR            _("Depreciation")
#define DESC_STR            _("Description")
#define DIFF_STR            _("Difference")
#define DIRECTDEBIT_STR     _("Direct Debit")
#define DIST_STR            _("Dist")    /* Distribution */
#define DIV_STR             _("Div")     /* Dividend */
#define DUPLICATE_STR_N    N_("Duplicate")
#define DUPLICATE_STR       _(DUPLICATE_STR_N)
#define EDIT_STR_N         N_("Edit")
#define EDIT_STR            _(EDIT_STR_N)
#define EQUITY_STR          _("Equity")
#define EXIT_STR_N         N_("Exit")
#define EXIT_STR            _(EXIT_STR_N)
#define EXPENSE_STR         _("Expense")
#define EXPORT_STR          _("Export")
#define EXTENSIONS_STR      _("Extensions")
#define FEE_STR             _("Fee")
#define FIELD_STR           _("Field")
#define FILE_STR            _("File")
#define FIND_STR_N         N_("Find")
#define FIND_STR            _(FIND_STR_N)
#define FORWARD_STR         _("Forward")
#define FROM_STR            _("From")
#define GNC_PREFS           _("GnuCash Preferences")
#define HELP_STR            _("Help")
#define IMBALANCE_STR       _("Imbalance")
#define IMPORT_STR_N       N_("Import")
#define IMPORT_STR          _(IMPORT_STR_N)
#define INCOME_STR          _("Income")
#define INCREASE_STR        _("Increase")
#define INT_STR             _("Int")     /* Interest */
#define JUMP_STR_N         N_("Jump")
#define JUMP_STR            _(JUMP_STR_N)
#define LIABILITY_STR       _("Liability")
#define LICENSE_STR         _("License")
#define LOAN_STR            _("Loan")   
#define LTCG_STR            _("LTCG")    /* Long Term Capital Gains */
#define MEMO_STR            _("Memo")
#define MONTHS_STR          _("Months")
#define NEW_STR_N          N_("New")
#define NEW_STR             _(NEW_STR_N)
#define NO_STR              _("No")
#define NONE_STR            _("(none)")
#define NOTES_STR           _("Notes")
#define NUM_STR             _("Num")
#define OK_STR              _("Ok")
#define ONLINE_STR          _("Online")
#define OPEN_STR_N         N_("Open")
#define OPEN_STR            _(OPEN_STR_N)
#define ORPHAN_STR          _("Orphan")
#define PARAMETERS_STR      _("Parameters")
#define PAYMENT_STR         _("Payment")
#define PORTFOLIO_STR       _("Portfolio")
#define POS_STR             _("POS")   /* Point of Sale credit card machine */
#define PREFERENCES_STR     _("Preferences")
#define PRICE_STR           _("Price")
#define PRINT_STR           _("Print")
#define PROFITS_STR         _("Profits")
#define QUIT_STR            _("Quit")
#define REBATE_STR          _("Rebate")
#define RECEIVE_STR         _("Receive")
#define RECONCILE_STR       _("Reconcile")
#define RECORD_STR_N       N_("Record")
#define RECORD_STR          _(RECORD_STR_N)
#define REGISTER_STR        _("Register")
#define REPORT_STR          _("Report")
#define REPORTS_STR         _("Reports")
#define SAVE_STR_N         N_("Save")
#define SAVE_STR            _(SAVE_STR_N)
#define SAVINGS_STR         _("Savings")
#define SCRUB_STR           _("Scrub")
#define SEARCH_RESULTS_STR  _("Search Results")
#define SECURITY_STR        _("Security")
#define SELL_STR            _("Sell")
#define SETTINGS_STR        _("Settings")
#define SIMPLE_STR          _("Simple")
#define SOLD_STR            _("Sold")
#define SPEND_STR           _("Spend")
#define SPLIT_STR           _("Split")
#define STCG_STR            _("STCG")   /* Short Term Captial Gains */
#define STOCK_STR           _("Stock")  
#define STYLE_STR           _("Style")
#define SURPLUS_STR         _("Surplus")
#define TELLER_STR          _("Teller")
#define TO_STR              _("To")
#define TODAY_STR           _("Today")
#define TOTAL_STR           _("Total")
#define TOTAL_EURO_STR      _("Total")
#define TYPE_STR            _("Type")
#define TRANSACTION_STR     _("Transaction")
#define TRANSFER_STR_N     N_("Transfer")
#define TRANSFER_STR        _(TRANSFER_STR_N)
#define VALUE_STR           _("Value")
#define WARN_STR            _("WARNING")
#define WEEKS_STR           _("Weeks")
#define WIRE_STR            _("Wire")         /* Electronic Wire transfer */
#define WITHDRAW_STR        _("Withdraw")
#define WITHDRAWAL_STR      _("Withdrawal")
#define YEARS_STR           _("Years")
#define YES_STR             _("Yes")


#endif /* __XACC_MESSAGES_I18N_H__ */
