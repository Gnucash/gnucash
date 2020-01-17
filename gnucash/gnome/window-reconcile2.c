/********************************************************************\
 * window-reconcile2.c -- the reconcile window                      *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1998-2000 Linas Vepstas                            *
 * Copyright (C) 2002 Christian Stimming                            *
 * Copyright (C) 2006 David Hampton                                 *
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
 *   Author: Rob Clark                                              *
 * Internet: rclark@cs.hmc.edu                                      *
 *  Address: 609 8th Street                                         *
 *           Huntington Beach, CA 92648-4632                        *
\********************************************************************/

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#ifdef __G_IR_SCANNER__
#undef __G_IR_SCANNER__
#endif
#include <gdk/gdkkeysyms.h>

#include "Scrub.h"
#include "Scrub3.h"
#include "dialog-account.h"
#include "dialog-transfer.h"
#include "dialog-utils.h"
#include "gnc-amount-edit.h"
#include "gnc-component-manager.h"
#include "gnc-date.h"
#include "gnc-date-edit.h"
#include "gnc-event.h"
#include "gnc-filepath-utils.h"
#include "gnc-gnome-utils.h"
#include "gnc-plugin-page-register2.h"
#include "gnc-prefs.h"
#include "gnc-ui.h"
#include "gnc-ui-balances.h"
#include "gnc-window.h"
#include "guile-util.h"
#include "reconcile-view.h"
#include "window-reconcile2.h"

#define WINDOW_RECONCILE_CM_CLASS "window-reconcile"
#define GNC_PREF_AUTO_INTEREST_TRANSFER "auto-interest-transfer"
#define GNC_PREF_AUTO_CC_PAYMENT        "auto-cc-payment"
#define GNC_PREF_ALWAYS_REC_TO_TODAY    "always-reconcile-to-today"


/** STRUCTS *********************************************************/
struct _RecnWindow2
{
    GncGUID account;             /* The account that we are reconciling  */
    gnc_numeric new_ending;      /* The new ending balance               */
    time64 statement_date;       /* The statement date                   */

    gint component_id;           /* id of component                      */

    GtkWidget *window;           /* The reconcile window                 */

    GtkUIManager *ui_merge;
    GtkActionGroup *action_group;

    GtkWidget *starting;         /* The starting balance                 */
    GtkWidget *ending;           /* The ending balance                   */
    GtkWidget *recn_date;        /* The statement date                   */
    GtkWidget *reconciled;       /* The reconciled balance               */
    GtkWidget *difference;       /* Text field, amount left to reconcile */

    GtkWidget *total_debit;      /* Text field, total debit reconciled   */
    GtkWidget *total_credit;     /* Text field, total credit reconciled  */

    GtkWidget *debit;            /* Debit matrix show unreconciled debit */
    GtkWidget *credit;           /* Credit matrix, shows credits...      */

    GtkWidget *debit_frame;      /* Frame around debit matrix            */
    GtkWidget *credit_frame;     /* Frame around credit matrix           */

    gboolean   delete_refresh;   /* do a refresh upon a window deletion  */
};


/* This structure doesn't contain everything involved in the
 * startRecnWindow, just pointers that have to be passed in to
 * callbacks that need more than one piece of data to operate on.
 * This is also used by the interest transfer dialog code.
 */
typedef struct _startRecnWindowData
{
    Account       *account;         /* the account being reconciled            */
    GNCAccountType account_type;    /* the type of the account                 */

    GtkWidget     *startRecnWindow; /* the startRecnWindow dialog              */
    GtkWidget     *xfer_button;     /* the dialog's interest transfer button   */
    GtkWidget     *date_value;      /* the dialog's ending date field          */
    GNCAmountEdit *end_value;       /* the dialog's ending balance amount edit */
    gnc_numeric    original_value;  /* the dialog's original ending balance    */
    gboolean       user_set_value;  /* the user changed the ending value       */

    XferDialog    *xferData;        /* the interest xfer dialog (if it exists) */
    gboolean       include_children;

    time64         date;            /* the interest xfer reconcile date        */
} startRecnWindowData;


/* Note: make sure to update the help text for this in prefs.scm if these
 * change!  These macros define the account types for which an auto interest
 * xfer dialog could pop up, if the user's preferences allow it.
 */
#define account_type_has_auto_interest_charge(type)  (((type) == ACCT_TYPE_CREDIT) || \
                                                      ((type) == ACCT_TYPE_LIABILITY) ||\
						      ((type) == ACCT_TYPE_PAYABLE))

#define account_type_has_auto_interest_payment(type) (((type) == ACCT_TYPE_BANK)  || \
                                                      ((type) == ACCT_TYPE_ASSET) || \
                                                      ((type) == ACCT_TYPE_MUTUAL) || \
						      ((type) == ACCT_TYPE_RECEIVABLE))

#define account_type_has_auto_interest_xfer(type) \
  (  account_type_has_auto_interest_charge(type) || \
    account_type_has_auto_interest_payment(type) )

/** PROTOTYPES ******************************************************/
static gnc_numeric recnRecalculateBalance (RecnWindow2 *recnData);

static void   recn_destroy_cb (GtkWidget *w, gpointer data);
static void   recn_cancel (RecnWindow2 *recnData);
static gboolean recn_delete_cb (GtkWidget *widget, GdkEvent *event, gpointer data);
static gboolean recn_key_press_cb (GtkWidget *widget, GdkEventKey *event, gpointer data);
static void   recnFinishCB (GtkAction *action, RecnWindow2 *recnData);
static void   recnPostponeCB (GtkAction *action, gpointer data);
static void   recnCancelCB (GtkAction *action, gpointer data);

void gnc_start_recn2_children_changed (GtkWidget *widget, startRecnWindowData *data);
void gnc_start_recn2_interest_clicked_cb (GtkButton *button, startRecnWindowData *data);

static void   gnc_reconcile_window_set_sensitivity (RecnWindow2 *recnData);
static char * gnc_recn_make_window_name (Account *account);
static void   gnc_recn_set_window_name (RecnWindow2 *recnData);
static gboolean find_by_account (gpointer find_data, gpointer user_data);


/** GLOBALS ************************************************************/
/* This static indicates the debugging module that this .o belongs to. */
G_GNUC_UNUSED static QofLogModule log_module = GNC_MOD_GUI;

static time64 gnc_reconcile_last_statement_date = 0;


/** IMPLEMENTATIONS *************************************************/

/** An array of all of the actions provided by the main window code.
 *  This includes some placeholder actions for the menus that are
 *  visible in the menu bar but have no action associated with
 *  them. */
static GtkActionEntry recnWindow2_actions [];
/** The number of actions provided by the main window. */
static guint recnWindow2_n_actions;

/********************************************************************\
 * recnRefresh                                                      *
 *   refreshes the transactions in the reconcile window             *
 *                                                                  *
 * Args:   account - the account of the reconcile window to refresh *
 * Return: none                                                     *
\********************************************************************/
static void
recnRefresh (RecnWindow2 *recnData)
{
    if (recnData == NULL)
        return;

    gnc_reconcile_view_refresh (GNC_RECONCILE_VIEW (recnData->debit));
    gnc_reconcile_view_refresh (GNC_RECONCILE_VIEW (recnData->credit));

    gnc_reconcile_window_set_sensitivity (recnData);

    gnc_recn_set_window_name (recnData);

    recnRecalculateBalance (recnData);

    gtk_widget_queue_resize (recnData->window);
}


static Account *
recn_get_account (RecnWindow2 *recnData)
{
    if (!recnData)
        return NULL;

    return xaccAccountLookup (&recnData->account, gnc_get_current_book ());
}


/********************************************************************\
 * recnRecalculateBalance                                           *
 *   refreshes the balances in the reconcile window                 *
 *                                                                  *
 * Args:   recnData -- the reconcile window to refresh              *
 * Return: the difference between the nominal ending balance        *
 *         and the 'effective' ending balance.                      *
\********************************************************************/
static gnc_numeric
recnRecalculateBalance (RecnWindow2 *recnData)
{
    Account *account;
    const char *amount;
    gnc_numeric debit;
    gnc_numeric credit;
    gnc_numeric starting;
    gnc_numeric ending;
    gnc_numeric reconciled;
    gnc_numeric diff;
    GNCPrintAmountInfo print_info;
    gboolean reverse_balance, include_children;
    GtkAction *action;

    account = recn_get_account (recnData);
    if (!account)
        return gnc_numeric_zero ();

    reverse_balance = gnc_reverse_balance (account);

    /* update the starting balance */
    include_children = xaccAccountGetReconcileChildrenStatus (account);
    starting = gnc_ui_account_get_reconciled_balance (account, include_children);
    print_info = gnc_account_print_info (account, TRUE);

    /*
     * Do not reverse the balance here. It messes up the math in the
     * reconciliation window. Also, the balance should show up as a
     * positive number in the reconciliation window to match the positive
     * number that shows in the register window.
     */

    amount = xaccPrintAmount (starting, print_info);
    gnc_set_label_color (recnData->starting, starting);
    gtk_label_set_text (GTK_LABEL (recnData->starting), amount);
    if (reverse_balance)
        starting = gnc_numeric_neg (starting);

    /* update the statement date */
    amount = qof_print_date (recnData->statement_date);
    gtk_label_set_text (GTK_LABEL (recnData->recn_date), amount);

    /* update the ending balance */
    ending = recnData->new_ending;
    if (reverse_balance)
        ending = gnc_numeric_neg (ending);
    amount = xaccPrintAmount (ending, print_info);
    gnc_set_label_color (recnData->ending, ending);
    gtk_label_set_text (GTK_LABEL (recnData->ending), amount);
    if (reverse_balance)
        ending = gnc_numeric_neg (ending);

    debit = gnc_reconcile_view_reconciled_balance
            (GNC_RECONCILE_VIEW (recnData->debit));

    credit = gnc_reconcile_view_reconciled_balance
             (GNC_RECONCILE_VIEW (recnData->credit));

    /* Update the total debit and credit fields */
    amount = xaccPrintAmount (debit, print_info);
    gtk_label_set_text (GTK_LABEL (recnData->total_debit), amount);

    amount = xaccPrintAmount (credit, print_info);

    gtk_label_set_text (GTK_LABEL (recnData->total_credit), amount);

    /* update the reconciled balance */
    reconciled = gnc_numeric_add_fixed (starting,
                                        gnc_numeric_sub_fixed (debit, credit));
    if (reverse_balance)
        reconciled = gnc_numeric_neg (reconciled);
    amount = xaccPrintAmount (reconciled, print_info);
    gnc_set_label_color (recnData->reconciled, reconciled);
    gtk_label_set_text (GTK_LABEL (recnData->reconciled), amount);
    if (reverse_balance)
        reconciled = gnc_numeric_neg (reconciled);

    /* update the difference */
    diff = gnc_numeric_sub_fixed (ending, reconciled);
    if (reverse_balance)
        diff = gnc_numeric_neg (diff);
    amount = xaccPrintAmount (diff, print_info);
    gnc_set_label_color (recnData->difference, diff);
    gtk_label_set_text (GTK_LABEL (recnData->difference), amount);
    if (reverse_balance)
        diff = gnc_numeric_neg (diff);

    action = gtk_action_group_get_action (recnData->action_group,
                                          "RecnFinishAction");
    gtk_action_set_sensitive (action, gnc_numeric_zero_p (diff));

    action = gtk_action_group_get_action (recnData->action_group,
                                          "TransBalanceAction");
    gtk_action_set_sensitive (action, !gnc_numeric_zero_p (diff));

    return diff;
}


static gboolean
gnc_start_recn2_update_cb (GtkWidget *widget, GdkEventFocus *event,
                         startRecnWindowData *data)
{
    gnc_numeric value;

    gnc_amount_edit_evaluate (GNC_AMOUNT_EDIT (data->end_value));

    value = gnc_amount_edit_get_amount (GNC_AMOUNT_EDIT (data->end_value));
    data->user_set_value = !gnc_numeric_equal (value, data->original_value);
    return FALSE;
}


/* If the user changed the date edit widget, update the
 * ending balance to reflect the ending balance of the account
 * on the date that the date edit was changed to.
 */
static void
gnc_start_recn2_date_changed (GtkWidget *widget, startRecnWindowData *data)
{
    GNCDateEdit *gde = GNC_DATE_EDIT (widget);
    gnc_numeric new_balance;
    time64 new_date;

    if (data->user_set_value)
        return;
    new_date = gnc_date_edit_get_date_end (gde);
    /* get the balance for the account as of the new date */
    new_balance = gnc_ui_account_get_balance_as_of_date (data->account, new_date,
                  data->include_children);
    /* update the amount edit with the amount */
    gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (data->end_value),
                                new_balance);
}


void
gnc_start_recn2_children_changed (GtkWidget *widget, startRecnWindowData *data)
{
    data->include_children =
        gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (widget));

    /* Force an update of the ending balance */
    gnc_start_recn2_date_changed (data->date_value, data);
}


/* For a given account, determine if an auto interest xfer dialog should be
 * shown, based on both the per-account flag as well as the global reconcile
 * option.  The global option is the default that is used if there is no
 * per-account option.
 */
static gboolean
gnc_recn_interest_xfer_get_auto_interest_xfer_allowed (Account *account)
{
    gboolean auto_xfer;

    auto_xfer = gnc_prefs_get_bool (GNC_PREFS_GROUP_RECONCILE, GNC_PREF_AUTO_INTEREST_TRANSFER);
    return xaccAccountGetAutoInterestXfer (account, auto_xfer);
}


/********************************************************************\
 * recnInterestXferWindow                                           *
 *   opens up a window to prompt the user to enter an interest      *
 *   charge or payment for an account prior to reconciling it.      *
 *   Only to be called for some types of accounts, as defined       *
 *   in the macros at the top of this file.                         *
 *                                                                  *
 * NOTE: This function does not return until the user presses "Ok"  *
 *       or "Cancel", which means that the transaction must be      *
 *       resolved before the startRecnWindow will work.             *
 *                                                                  *
 * Args:   data           - jumbo structure containing info         *
 *                          about the start of the reconcile        *
 *                          process needed by this function.        *
 * Returns:  none.                                                  *
\********************************************************************/

/* helper function */
static char *
gnc_recn_make_interest_window_name (Account *account, char *text)
{
    char *fullname;
    char *title;

    fullname = gnc_account_get_full_name (account);
    title = g_strconcat (fullname, " - ", text && *text ? _(text) : "", NULL);

    g_free (fullname);

    return title;
}


/* user clicked button in the interest xfer dialog entitled
 * "No Auto Interest Payments for this Account".
 */
static void
gnc_recn_interest_xfer_no_auto_clicked_cb (GtkButton *button,
        startRecnWindowData *data)
{
    /* Indicate that the user doesn't want
     * an auto interest xfer for this account.
     */
    xaccAccountSetAutoInterestXfer (data->account, FALSE);

    /* shut down the interest xfer dialog */
    gnc_xfer_dialog_close (data->xferData);

    /* make the button clickable again */
    if (data->xfer_button)
        gtk_widget_set_sensitive (GTK_WIDGET (data->xfer_button), TRUE);
}


static void
recnInterestXferWindow (startRecnWindowData *data)
{
    gchar *title;

    if (!account_type_has_auto_interest_xfer (data->account_type)) return;

    /* get a normal transfer dialog... */
    data->xferData = gnc_xfer_dialog (GTK_WIDGET(data->startRecnWindow),
                                      data->account);

    /* ...and start changing things: */

    /* change title */
    if ( account_type_has_auto_interest_payment (data->account_type))
        title = gnc_recn_make_interest_window_name (data->account,
                _("Interest Payment"));
    else
        title = gnc_recn_make_interest_window_name (data->account,
                _("Interest Charge"));

    gnc_xfer_dialog_set_title (data->xferData, title);
    g_free (title);


    /* change frame labels */
    gnc_xfer_dialog_set_information_label (data->xferData,
                                           _("Payment Information"));

    /* Interest accrued is a transaction from an income account
     * to a bank account.  Interest charged is a transaction from
     * a credit account to an expense account.  The user isn't allowed
     * to change the account (bank or credit) being reconciled.
     */
    if (account_type_has_auto_interest_payment (data->account_type))
    {
        gnc_xfer_dialog_set_from_account_label (data->xferData,
                                                _("Payment From"));
        gnc_xfer_dialog_set_from_show_button_active( data->xferData, TRUE);

        // XXX: Set "from" account from previous interest payment.

        gnc_xfer_dialog_set_to_account_label (data->xferData,
                                              _("Reconcile Account"));
        gnc_xfer_dialog_select_to_account (data->xferData, data->account);
        gnc_xfer_dialog_lock_to_account_tree (data->xferData );

        /* Quickfill based on the reconcile account, which is the "To" acct. */
        gnc_xfer_dialog_quickfill_to_account (data->xferData, TRUE);
    }
    else  /* interest charged to account rather than paid to it */
    {
        gnc_xfer_dialog_set_from_account_label (data->xferData,
                                                _("Reconcile Account"));
        gnc_xfer_dialog_select_from_account (data->xferData, data->account);
        gnc_xfer_dialog_lock_from_account_tree (data->xferData);

        gnc_xfer_dialog_set_to_account_label (data->xferData,
                                              _("Payment To"));
        gnc_xfer_dialog_set_to_show_button_active (data->xferData, TRUE);

        // XXX: Set "to" account from previous interest payment.

        /* Quickfill based on the reconcile account, which is the "From" acct. */
        gnc_xfer_dialog_quickfill_to_account (data->xferData, FALSE);
    }


    /* add a button to disable auto interest payments for this account */
    gnc_xfer_dialog_add_user_specified_button (data->xferData,
            ( account_type_has_auto_interest_payment (data->account_type) ?
              _("No Auto Interest Payments for this Account")
              : _("No Auto Interest Charges for this Account")),
            G_CALLBACK (gnc_recn_interest_xfer_no_auto_clicked_cb),
            (gpointer) data );

    /* no currency frame */
    gnc_xfer_dialog_toggle_currency_table (data->xferData, FALSE);

    /* set the reconcile date for the transaction date */
    gnc_xfer_dialog_set_date (data->xferData, data->date);

    /* Now run the transfer dialog.  This blocks until done.
     * If the user hit Cancel, make the button clickable so that
     * the user can retry if they want.  We don't make the button
     * clickable if they successfully entered a transaction, since
     * the fact that the button was clickable again might make
     * the user think that the transaction didn't actually go through.
     */
    if (!gnc_xfer_dialog_run_until_done (data->xferData))
        if (data->xfer_button)
            gtk_widget_set_sensitive (GTK_WIDGET (data->xfer_button), TRUE);

    /* done with the XferDialog */
    data->xferData = NULL;
}


/* Set up for the interest xfer window, run the window, and update
 * the startRecnWindow if the interest xfer changed anything that matters.
 */
static void
gnc_reconcile_interest_xfer_run (startRecnWindowData *data)
{
    GtkWidget *entry = gnc_amount_edit_gtk_entry (
                           GNC_AMOUNT_EDIT(data->end_value));
    gnc_numeric before = gnc_amount_edit_get_amount (
                             GNC_AMOUNT_EDIT(data->end_value));
    gnc_numeric after;

    recnInterestXferWindow (data);

    /* recompute the ending balance */
    after = xaccAccountGetBalanceAsOfDate (data->account, data->date);

    /* update the ending balance in the startRecnWindow if it has changed. */
    if ( gnc_numeric_compare (before, after))
    {
        if (gnc_reverse_balance (data->account))
            after = gnc_numeric_neg (after);

        gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (data->end_value), after);
        gtk_widget_grab_focus (GTK_WIDGET (entry));
        gtk_editable_select_region (GTK_EDITABLE (entry), 0, -1);
        data->original_value = after;
        data->user_set_value = FALSE;
    }
}


void
gnc_start_recn2_interest_clicked_cb (GtkButton *button, startRecnWindowData *data)
{
    /* indicate in account that user wants
     * an auto interest xfer for this account */
    xaccAccountSetAutoInterestXfer (data->account, TRUE);

    /* make the button unclickable since we're popping up the window */
    if (data->xfer_button)
        gtk_widget_set_sensitive (GTK_WIDGET (data->xfer_button), FALSE);

    /* run the account window */
    gnc_reconcile_interest_xfer_run (data);
}


static void
gnc_save_reconcile_interval (Account *account, time64 statement_date)
{
    time64 prev_statement_date;
    int days = 0, months = 0;
    double seconds;

    if (!xaccAccountGetReconcileLastDate (account, &prev_statement_date))
        return;

    /*
     * Compute the number of days difference.
     */
    seconds = gnc_difftime (statement_date, prev_statement_date);
    days = (int)(seconds / 60 / 60 / 24);

    /*
     * See if we need to remember days(weeks) or months.  The only trick
     * value is 28 days which could be wither 4 weeks or 1 month.
     */
    if (days == 28)
    {
        int prev_days = 0, prev_months = 1;

        /* What was it last time? */
        xaccAccountGetReconcileLastInterval (account, &prev_months, &prev_days);
        if (prev_months == 1)
        {
            months = 1;
            days = 0;
        }
    }
    else if (days > 28)
    {
        struct tm current, prev;

        gnc_localtime_r (&statement_date, &current);
        gnc_localtime_r (&prev_statement_date, &prev);
        months = ((12 * current.tm_year + current.tm_mon) -
                  (12 * prev.tm_year + prev.tm_mon));
        days = 0;
    }

    /*
     * Remember for next time unless it is negative.
     */
    if (months >= 0 && days >= 0)
        xaccAccountSetReconcileLastInterval (account, months, days);
}


/********************************************************************\
 * startRecnWindow                                                  *
 *   opens up the window to prompt the user to enter the ending     *
 *   balance from bank statement                                    *
 *                                                                  *
 * NOTE: This function does not return until the user presses "Ok"  *
 *       or "Cancel"                                                *
 *                                                                  *
 * Args:   parent         - the parent of this window               *
 *         account        - the account to reconcile                *
 *         new_ending     - returns the amount for ending balance   *
 *         statement_date - returns date of the statement :)        *
 * Return: True, if the user presses "Ok", else False               *
\********************************************************************/
static gboolean
startRecnWindow (GtkWidget *parent, Account *account,
                gnc_numeric *new_ending, time64 *statement_date,
                gboolean enable_subaccount)
{
    GtkWidget *dialog, *end_value, *date_value, *include_children_button;
    GtkBuilder *builder;
    startRecnWindowData data = { NULL };
    gboolean auto_interest_xfer_option;
    GNCPrintAmountInfo print_info;
    gnc_numeric ending;
    char *title;
    int result;

    /* Initialize the data structure that will be used for several callbacks
     * throughout this file with the relevant info.  Some initialization is
     * done below as well.  Note that local storage should be OK for this,
     * since any callbacks using it will only work while the startRecnWindow
     * is running.
     */
    data.account = account;
    data.account_type = xaccAccountGetType (account);
    data.date = *statement_date;

    /* whether to have an automatic interest xfer dialog or not */
    auto_interest_xfer_option =
        gnc_recn_interest_xfer_get_auto_interest_xfer_allowed (account);

    data.include_children = xaccAccountGetReconcileChildrenStatus (account);

    ending = gnc_ui_account_get_reconciled_balance (account,
             data.include_children);
    print_info = gnc_account_print_info (account, TRUE);

    /*
     * Do not reverse the balance here.  It messes up the math in the
     * reconciliation window.  Also, the balance should show up as a
     * positive number in the reconciliation window to match the positive
     * number that shows in the register window.
     */

    /* Create the dialog box */
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "window-reconcile.glade", "reconcile_start_dialog");

    dialog = GTK_WIDGET(gtk_builder_get_object (builder, "reconcile_start_dialog"));

    // Set the style context for this dialog so it can be easily manipulated with css
    gnc_widget_set_style_context (GTK_WIDGET(dialog), "GncReconcileDialog");

    title = gnc_recn_make_window_name (account);
    gtk_window_set_title (GTK_WINDOW (dialog), title);
    g_free (title);

    data.startRecnWindow = GTK_WIDGET (dialog);

    if (parent != NULL)
        gtk_window_set_transient_for (GTK_WINDOW (dialog), GTK_WINDOW (parent));

    {
        GtkWidget *start_value, *box;
        GtkWidget *entry, *label;
        GtkWidget *interest = NULL;

        start_value = GTK_WIDGET (gtk_builder_get_object (builder, "start_value"));
        gtk_label_set_text (GTK_LABEL(start_value), xaccPrintAmount (ending, print_info));

        include_children_button = GTK_WIDGET (gtk_builder_get_object (builder, "subaccount_check"));
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (include_children_button),
                                     data.include_children);
        gtk_widget_set_sensitive (include_children_button, enable_subaccount);

        date_value = gnc_date_edit_new (*statement_date, FALSE, FALSE);
        data.date_value = date_value;
        box = GTK_WIDGET (gtk_builder_get_object (builder, "date_value_box"));
        gtk_box_pack_start (GTK_BOX (box), date_value, TRUE, TRUE, 0);
        label = GTK_WIDGET (gtk_builder_get_object (builder, "date_label"));
        gnc_date_make_mnemonic_target (GNC_DATE_EDIT (date_value), label);

        end_value = gnc_amount_edit_new ();
        data.end_value = GNC_AMOUNT_EDIT (end_value);
        data.original_value = *new_ending;
        data.user_set_value = FALSE;
        box = GTK_WIDGET (gtk_builder_get_object (builder, "ending_value_box"));
        gtk_box_pack_start (GTK_BOX (box), end_value, TRUE, TRUE, 0);
        label = GTK_WIDGET (gtk_builder_get_object (builder, "end_label"));
        gtk_label_set_mnemonic_widget (GTK_LABEL (label), end_value);

        gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, &data);

        gnc_date_activates_default (GNC_DATE_EDIT (date_value), TRUE);

        /* need to get a callback on date changes to update the recn balance */
        g_signal_connect ( G_OBJECT (date_value), "date_changed",
                           G_CALLBACK (gnc_start_recn2_date_changed), (gpointer) &data );

        print_info.use_symbol = 0;
        gnc_amount_edit_set_print_info (GNC_AMOUNT_EDIT (end_value), print_info);
        gnc_amount_edit_set_fraction (GNC_AMOUNT_EDIT (end_value),
                                      xaccAccountGetCommoditySCU (account));

        gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (end_value), *new_ending);

        entry = gnc_amount_edit_gtk_entry (GNC_AMOUNT_EDIT (end_value));
        gtk_editable_select_region (GTK_EDITABLE (entry), 0, -1);
        g_signal_connect (G_OBJECT (entry), "focus-out-event",
                         G_CALLBACK (gnc_start_recn2_update_cb), (gpointer) &data);
        gtk_entry_set_activates_default (GTK_ENTRY (entry), TRUE);

        /* if it's possible to enter an interest payment or charge for this
         * account, add a button so that the user can pop up the appropriate
         * dialog if it isn't automatically popping up.
         */
        interest = GTK_WIDGET(gtk_builder_get_object (builder, "interest_button"));
        if ( account_type_has_auto_interest_payment( data.account_type ) )
            gtk_button_set_label (GTK_BUTTON (interest), _("Enter _Interest Payment...") );
        else if ( account_type_has_auto_interest_charge( data.account_type ) )
            gtk_button_set_label (GTK_BUTTON (interest), _("Enter _Interest Charge...") );
        else
        {
            gtk_widget_destroy (interest);
            interest = NULL;
        }

        if ( interest )
        {
            data.xfer_button = interest;
            if ( auto_interest_xfer_option )
                gtk_widget_set_sensitive (GTK_WIDGET (interest), FALSE);
        }

        gtk_widget_show_all (dialog);

        gtk_widget_grab_focus(gnc_amount_edit_gtk_entry
                              (GNC_AMOUNT_EDIT (end_value)));
    }

    /* Allow the user to enter an interest payment
     * or charge prior to reconciling */
    if (account_type_has_auto_interest_xfer (data.account_type)
            && auto_interest_xfer_option)
    {
        gnc_reconcile_interest_xfer_run (&data);
    }

    result = gtk_dialog_run (GTK_DIALOG (dialog));
    if (result == GTK_RESPONSE_OK)
    {
        *new_ending = gnc_amount_edit_get_amount (GNC_AMOUNT_EDIT (end_value));
        *statement_date = gnc_date_edit_get_date_end (GNC_DATE_EDIT (date_value));

        if (gnc_reverse_balance (account))
            *new_ending = gnc_numeric_neg (*new_ending);

        xaccAccountSetReconcileChildrenStatus (account, data.include_children);

        gnc_save_reconcile_interval (account, *statement_date);
    }
    gtk_widget_destroy (dialog);
    g_object_unref (G_OBJECT (builder));

    return (result == GTK_RESPONSE_OK);
}


static void
gnc_reconcile_window_set_sensitivity (RecnWindow2 *recnData)
{
    gboolean sensitive = FALSE;
    GNCReconcileView *view;
    GtkAction *action;

    view = GNC_RECONCILE_VIEW (recnData->debit);
    if (gnc_reconcile_view_num_selected (view) == 1)
        sensitive = TRUE;

    view = GNC_RECONCILE_VIEW (recnData->credit);
    if (gnc_reconcile_view_num_selected (view) == 1)
        sensitive = TRUE;

    action = gtk_action_group_get_action (recnData->action_group,
                                          "TransEditAction");
    gtk_action_set_sensitive (action, sensitive);
    action = gtk_action_group_get_action (recnData->action_group,
                                          "TransDeleteAction");
    gtk_action_set_sensitive (action, sensitive);

    sensitive = FALSE;

    view = GNC_RECONCILE_VIEW (recnData->debit);
    if (gnc_reconcile_view_num_selected (view) > 0)
        sensitive = TRUE;

    view = GNC_RECONCILE_VIEW (recnData->credit);
    if (gnc_reconcile_view_num_selected (view) > 0)
        sensitive = TRUE;

    action = gtk_action_group_get_action (recnData->action_group,
                                          "TransRecAction");
    gtk_action_set_sensitive (action, sensitive);
    action = gtk_action_group_get_action (recnData->action_group,
                                          "TransUnRecAction");
    gtk_action_set_sensitive (action, sensitive);
}


static void
gnc_reconcile_window_toggled_cb (GNCReconcileView *view, Split *split,
                             gpointer data)
{
    RecnWindow2 *recnData = data;
    gnc_reconcile_window_set_sensitivity (recnData);
    recnRecalculateBalance (recnData);
}


static void
gnc_reconcile_window_row_cb (GNCReconcileView *view, gpointer item,
                             gpointer data)
{
    RecnWindow2 *recnData = data;
    gnc_reconcile_window_set_sensitivity (recnData);
}


/** Popup a contextual menu.  This function ends up being called when
 *  the user right-clicks in the context of a window, or uses the
 *  keyboard context-menu request key combination (Shift-F10 by
 *  default).
 *
 *  @param recnData This is a data structure describing the
 *  Reconciliation Window.
 *
 *  @param event The event parameter passed to the "button-press"
 *  callback.  May be null if there was no event (aka keyboard
 *  request).
 */
static void
do_popup_menu (RecnWindow2 *recnData, GdkEventButton *event)
{
    GtkWidget *menu;
    int button, event_time;

    menu = gtk_ui_manager_get_widget (recnData->ui_merge, "/MainPopup");
    if (!menu)
    {
        return;
    }

#if GTK_CHECK_VERSION(3,22,0)
    gtk_menu_popup_at_pointer (GTK_MENU(menu), (GdkEvent *) event);
#else
    if (event)
    {
        button = event->button;
        event_time = event->time;
    }
    else
    {
        button = 0;
        event_time = gtk_get_current_event_time ();
    }

    gtk_menu_popup(GTK_MENU(menu), NULL, NULL, NULL, NULL, button, event_time);
#endif
}


/** Callback function invoked when the user requests that Gnucash
 *  popup the contextual menu via the keyboard context-menu request
 *  key combination (Shift-F10 by default).
 *
 *  @param recnData This is a data structure describing the
 *  Reconciliation Window.
 *
 *  @param widget Whatever widget had focus when the user issued the
 *  keyboard context-menu request.
 *
 *  @return Always returns TRUE to indicate that the menu request was
 *  handled.
 */
static gboolean
gnc_reconcile_window_popup_menu_cb (GtkWidget *widget,
                                    RecnWindow2 *recnData)
{
    do_popup_menu (recnData, NULL);
    return TRUE;
}


/*  Callback function invoked when the user clicks in the content of
 *  any Gnucash window.  If this was a "right-click" then Gnucash will
 *  popup the contextual menu.
 */
static gboolean
gnc_reconcile_window_button_press_cb (GtkWidget *widget,
                                      GdkEventButton *event,
                                      RecnWindow2 *recnData)
{
    GNCQueryView      *qview = GNC_QUERY_VIEW (widget);
    GtkTreeSelection  *selection;
    GtkTreePath       *path;

    if (event->button == 3 && event->type == GDK_BUTTON_PRESS)
    {

        /* Get tree path for row that was clicked */
        gtk_tree_view_get_path_at_pos (GTK_TREE_VIEW (qview),
                                             (gint) event->x, 
                                             (gint) event->y,
                                             &path, NULL, NULL, NULL);

        selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (qview));
        gtk_tree_selection_select_path (selection, path);
        gtk_tree_path_free (path);
        do_popup_menu (recnData, event);
        return TRUE;
    }
    return FALSE;
}


static GNCSplitReg2 *
gnc_reconcile_window_open_register (RecnWindow2 *recnData)
{
    Account *account = recn_get_account (recnData);
    GncPluginPage *page;
    GNCSplitReg2 *gsr;
    gboolean include_children;

    if (!account)
        return(NULL);

    include_children = xaccAccountGetReconcileChildrenStatus (account);
    page = gnc_plugin_page_register2_new (account, include_children);
    gnc_main_window_open_page (NULL, page);
    gsr = gnc_plugin_page_register2_get_gsr (page);
    gnc_split_reg2_raise (gsr);
    return gsr;
}


static void
gnc_reconcile_window_double_click_cb (GNCReconcileView *view, Split *split,
                                     gpointer data)
{
    RecnWindow2 *recnData = data;
    GNCSplitReg2 *gsr;

    /* This should never be true, but be paranoid */
    if (split == NULL)
        return;

    gsr = gnc_reconcile_window_open_register (recnData);
    if (gsr == NULL)
        return;
    gnc_split_reg2_jump_to_split (gsr, split);
}


static void
gnc_reconcile_window_focus_cb (GtkWidget *widget, GdkEventFocus *event,
                              gpointer data)
{
    RecnWindow2 *recnData = data;
    GNCReconcileView *this_view, *other_view;
    GNCReconcileView *debit, *credit;

    this_view = GNC_RECONCILE_VIEW (widget);

    debit  = GNC_RECONCILE_VIEW (recnData->debit);
    credit = GNC_RECONCILE_VIEW (recnData->credit);

    other_view = GNC_RECONCILE_VIEW (this_view == debit ? credit : debit);

    /* clear the *other* list so we always have no more than one selection */
    gnc_reconcile_view_unselect_all (other_view);
}


static gboolean
gnc_reconcile_key_press_cb (GtkWidget *widget, GdkEventKey *event,
                            gpointer data)
{
    RecnWindow2 *recnData = data;
    GtkWidget *this_view, *other_view;
    GtkWidget *debit, *credit;

    switch (event->keyval)
    {
    case GDK_KEY_Tab:
    case GDK_KEY_ISO_Left_Tab:
        break;

    default:
        return FALSE;
    }

    g_signal_stop_emission_by_name (widget, "key_press_event");

    this_view = widget;

    debit  = recnData->debit;
    credit = recnData->credit;

    other_view = (this_view == debit ? credit : debit);

    gtk_widget_grab_focus (other_view);

    return TRUE;
}


static void
gnc_reconcile_window_set_titles (RecnWindow2 *recnData)
{
    gboolean formal;
    gchar *title;

    formal = gnc_prefs_get_bool(GNC_PREFS_GROUP_GENERAL, GNC_PREF_ACCOUNTING_LABELS);

    if (formal)
        title = _("Debits");
    else
        title = gnc_get_debit_string (ACCT_TYPE_NONE);

    gtk_frame_set_label (GTK_FRAME (recnData->debit_frame), title);

    if (!formal)
        g_free(title);

    if (formal)
        title = _("Credits");
    else
        title = gnc_get_credit_string (ACCT_TYPE_NONE);

    gtk_frame_set_label (GTK_FRAME (recnData->credit_frame), title);

    if (!formal)
        g_free(title);
}


static GtkWidget *
gnc_reconcile_window_create_view_box (Account *account,
                                     GNCReconcileViewType type,
                                     RecnWindow2 *recnData,
                                     GtkWidget **list_save,
                                     GtkWidget **total_save)
{
    GtkWidget *frame, *scrollWin, *view, *vbox, *label, *hbox;

    frame = gtk_frame_new (NULL);

    if (type == RECLIST_DEBIT)
        recnData->debit_frame = frame;
    else
        recnData->credit_frame = frame;

    vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_set_homogeneous (GTK_BOX (vbox), FALSE);

    view = gnc_reconcile_view_new(account, type, recnData->statement_date);
    *list_save = view;

    g_signal_connect (view, "toggle_reconciled",
                     G_CALLBACK (gnc_reconcile_window_toggled_cb),
                     recnData);
    g_signal_connect (view, "line_selected",
                     G_CALLBACK (gnc_reconcile_window_row_cb),
                     recnData);
    g_signal_connect (view, "button_press_event",
                     G_CALLBACK (gnc_reconcile_window_button_press_cb),
                     recnData);
    g_signal_connect (view, "double_click_split",
                     G_CALLBACK (gnc_reconcile_window_double_click_cb),
                     recnData);
    g_signal_connect (view, "focus_in_event",
                     G_CALLBACK (gnc_reconcile_window_focus_cb),
                     recnData);
    g_signal_connect (view, "key_press_event",
                     G_CALLBACK (gnc_reconcile_key_press_cb),
                     recnData);

    scrollWin = gtk_scrolled_window_new (NULL, NULL);
    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrollWin),
                                   GTK_POLICY_AUTOMATIC,
                                   GTK_POLICY_AUTOMATIC);
    gtk_container_set_border_width (GTK_CONTAINER (scrollWin), 5);

    gtk_container_add (GTK_CONTAINER (frame), scrollWin);
    gtk_container_add (GTK_CONTAINER (scrollWin), view);
    gtk_box_pack_start (GTK_BOX (vbox), frame, TRUE, TRUE, 0);

    hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 5);
    gtk_box_set_homogeneous (GTK_BOX (hbox), FALSE);
    gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, FALSE, 0);

    label = gtk_label_new (_("Total"));
    gnc_label_set_alignment (label, 1.0, 0.5);
    gtk_box_pack_start (GTK_BOX (hbox), label, TRUE, TRUE, 0);

    label = gtk_label_new("");
    gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, FALSE, 0);
    *total_save = label;

#if GTK_CHECK_VERSION(3,12,0)
    gtk_widget_set_margin_end (GTK_WIDGET(label), 10);
#else
    gtk_widget_set_margin_right (GTK_WIDGET(label), 10);
#endif

    return vbox;
}


static Split *
gnc_reconcile_window_get_current_split (RecnWindow2 *recnData)
{
    GNCReconcileView *view;
    Split *split;

    view = GNC_RECONCILE_VIEW (recnData->debit);
    split = gnc_reconcile_view_get_current_split (view);
    if (split != NULL)
        return split;

    view = GNC_RECONCILE_VIEW (recnData->credit);
    split = gnc_reconcile_view_get_current_split (view);

    return split;
}


static void
gnc_ui_reconcile_window_help_cb (GtkWidget *widget, gpointer data)
{
    gnc_gnome_help (HF_HELP, HL_RECNWIN);
}


static void
gnc_ui_reconcile_window_change_cb (GtkAction *action, gpointer data)
{
    RecnWindow2 *recnData = data;
    Account *account = recn_get_account (recnData);
    gnc_numeric new_ending = recnData->new_ending;
    time64 statement_date = recnData->statement_date;

    if (gnc_reverse_balance (account))
        new_ending = gnc_numeric_neg (new_ending);
    if (startRecnWindow (recnData->window, account, &new_ending, &statement_date,
                         FALSE))
    {
        recnData->new_ending = new_ending;
        recnData->statement_date = statement_date;
        recnRecalculateBalance (recnData);
    }
}


static void
gnc_ui_reconcile_window_balance_cb (GtkButton *button, gpointer data)
{
    RecnWindow2 *recnData = data;
    GNCSplitReg2 *gsr;
    Account *account;
    gnc_numeric balancing_amount;
    time64 statement_date;


    gsr = gnc_reconcile_window_open_register (recnData);
    if (gsr == NULL)
        return;

    account = recn_get_account (recnData);
    if (account == NULL)
        return;

    balancing_amount = recnRecalculateBalance (recnData);
    if (gnc_numeric_zero_p (balancing_amount))
        return;

    statement_date = recnData->statement_date;
    if (statement_date == 0)
        statement_date = gnc_time (NULL); // default to 'now'

    gnc_split_reg2_balancing_entry (gsr, account, statement_date, balancing_amount);
}


static void
gnc_ui_reconcile_window_rec_cb (GtkButton *button, gpointer data)
{
    RecnWindow2 *recnData = data;
    GNCReconcileView *debit, *credit;

    debit  = GNC_RECONCILE_VIEW (recnData->debit);
    credit = GNC_RECONCILE_VIEW (recnData->credit);

    gnc_reconcile_view_set_list (debit, TRUE);
    gnc_reconcile_view_set_list (credit, TRUE);
}


static void
gnc_ui_reconcile_window_unrec_cb(GtkButton *button, gpointer data)
{
    RecnWindow2 *recnData = data;
    GNCReconcileView *debit, *credit;

    debit  = GNC_RECONCILE_VIEW (recnData->debit);
    credit = GNC_RECONCILE_VIEW (recnData->credit);

    gnc_reconcile_view_set_list (debit, FALSE);
    gnc_reconcile_view_set_list (credit, FALSE);
}


static void
gnc_ui_reconcile_window_delete_cb (GtkButton *button, gpointer data)
{
    RecnWindow2 *recnData = data;
    Transaction *trans;
    Split *split;

    split = gnc_reconcile_window_get_current_split (recnData);
    /* This should never be true, but be paranoid */
    if (split == NULL)
        return;

    {
        const char *message = _("Are you sure you want to delete the selected "
                                "transaction?");
        gboolean result;

        result = gnc_verify_dialog (GTK_WINDOW (recnData->window), FALSE, "%s", message);

        if (!result)
            return;
    }

    gnc_suspend_gui_refresh ();

    trans = xaccSplitGetParent (split);
    xaccTransDestroy (trans);

    gnc_resume_gui_refresh ();
}


static void
gnc_ui_reconcile_window_edit_cb (GtkButton *button, gpointer data)
{
    RecnWindow2 *recnData = data;
    GNCSplitReg2 *gsr;
    Split *split;

    split = gnc_reconcile_window_get_current_split (recnData);
    /* This should never be true, but be paranoid */
    if (split == NULL)
        return;

    gsr = gnc_reconcile_window_open_register (recnData);
    if (gsr == NULL)
        return;
    gnc_split_reg2_jump_to_split_amount (gsr, split);
}


static char *
gnc_recn_make_window_name (Account *account)
{
    char *fullname;
    char *title;

    fullname = gnc_account_get_full_name (account);
    title = g_strconcat (fullname, " - ", _("Reconcile"), NULL);

    g_free (fullname);

    return title;
}


static void
gnc_recn_set_window_name (RecnWindow2 *recnData)
{
    char *title;

    title = gnc_recn_make_window_name (recn_get_account (recnData));

    gtk_window_set_title (GTK_WINDOW (recnData->window), title);

    g_free (title);
}


static void
gnc_recn_edit_account_cb (GtkAction *action, gpointer data)
{
    RecnWindow2 *recnData = data;
    Account *account = recn_get_account (recnData);

    if (account == NULL)
        return;

    gnc_ui_edit_account_window (GTK_WINDOW (recnData->window), account);
}


static void
gnc_recn_xfer_cb (GtkAction *action, gpointer data)
{
    RecnWindow2 *recnData = data;
    Account *account = recn_get_account (recnData);

    if (account == NULL)
        return;

    gnc_xfer_dialog (recnData->window, account);
}


static void
gnc_recn_scrub_cb (GtkAction *action, gpointer data)
{
    RecnWindow2 *recnData = data;
    Account *account = recn_get_account (recnData);

    if (account == NULL)
        return;

    gnc_suspend_gui_refresh ();

    xaccAccountTreeScrubOrphans (account, gnc_window_show_progress);
    xaccAccountTreeScrubImbalance (account, gnc_window_show_progress);

    // XXX: Lots are disabled.
    if (g_getenv("GNC_AUTO_SCRUB_LOTS") != NULL)
        xaccAccountTreeScrubLots(account);

    gnc_resume_gui_refresh ();
}


static void
gnc_recn_open_cb (GtkAction *action, gpointer data)
{
    RecnWindow2 *recnData = data;

    gnc_reconcile_window_open_register (recnData);
}


static void
gnc_get_reconcile_info (Account *account,
                        gnc_numeric *new_ending,
                        time64 *statement_date)
{
    gboolean always_today;
    GDate date;
    time64 today;

    g_date_clear(&date, 1);

    always_today = gnc_prefs_get_bool (GNC_PREFS_GROUP_RECONCILE, GNC_PREF_ALWAYS_REC_TO_TODAY);

    if (!always_today &&
            xaccAccountGetReconcileLastDate (account, statement_date))
    {
        int months = 1, days = 0;

        gnc_gdate_set_time64 (&date, *statement_date);

        xaccAccountGetReconcileLastInterval (account, &months, &days);

        if (months)
        {
            gboolean was_last_day_of_month = g_date_is_last_of_month (&date);

            g_date_add_months (&date, months);

            /* Track last day of the month, i.e. 1/31 -> 2/28 -> 3/31 */
            if (was_last_day_of_month)
            {
                g_date_set_day (&date, g_date_get_days_in_month (g_date_get_month (&date),
                               g_date_get_year (&date)));
            }
        }
        else
        {
            g_date_add_days (&date, days);
        }

	*statement_date = gnc_time64_get_day_end_gdate (&date);

        today = gnc_time64_get_day_end (gnc_time (NULL));
        if (*statement_date > today)
            *statement_date = today;
    }

    xaccAccountGetReconcilePostponeDate (account, statement_date);

    if (xaccAccountGetReconcilePostponeBalance (account, new_ending))
    {
        if (gnc_reverse_balance (account))
            *new_ending = gnc_numeric_neg (*new_ending);
    }
    else
    {
        /* if the account wasn't previously postponed, try to predict
         * the statement balance based on the statement date.
         */
        *new_ending =
            gnc_ui_account_get_balance_as_of_date
            (account, *statement_date,
             xaccAccountGetReconcileChildrenStatus (account));
    }
}


static gboolean
find_by_account (gpointer find_data, gpointer user_data)
{
    Account *account = find_data;
    RecnWindow2 *recnData = user_data;

    if (!recnData)
        return FALSE;

    return guid_equal (&recnData->account, xaccAccountGetGUID (account));
}


static void
recn_set_watches_one_account (gpointer data, gpointer user_data)
{
    Account *account = (Account *)data;
    RecnWindow2 *recnData = (RecnWindow2 *)user_data;
    GList *node;

    for (node = xaccAccountGetSplitList (account); node; node = node->next)
    {
        Split *split = node->data;
        Transaction *trans;
        char recn;

        recn = xaccSplitGetReconcile (split);
        switch (recn)
        {
        case NREC:
        case CREC:
            trans = xaccSplitGetParent (split);

            gnc_gui_component_watch_entity (recnData->component_id,
                                            xaccTransGetGUID (trans),
                                            QOF_EVENT_MODIFY
                                            | QOF_EVENT_DESTROY
                                            | GNC_EVENT_ITEM_CHANGED);
            break;

        default:
            break;
        }
    }
}


static void
recn_set_watches (RecnWindow2 *recnData)
{
    gboolean include_children;
    Account *account;
    GList *accounts = NULL;

    gnc_gui_component_clear_watches (recnData->component_id);

    gnc_gui_component_watch_entity (recnData->component_id,
                                    &recnData->account,
                                    QOF_EVENT_MODIFY | QOF_EVENT_DESTROY);

    account = recn_get_account (recnData);

    include_children = xaccAccountGetReconcileChildrenStatus (account);
    if (include_children)
        accounts = gnc_account_get_descendants (account);

    /* match the account */
    accounts = g_list_prepend (accounts, account);

    g_list_foreach (accounts, recn_set_watches_one_account, recnData);

    g_list_free (accounts);
}


static void
refresh_handler (GHashTable *changes, gpointer user_data)
{
    RecnWindow2 *recnData = user_data;
    const EventInfo *info;
    Account *account;

    account = recn_get_account (recnData);
    if (!account)
    {
        gnc_close_gui_component_by_data (WINDOW_RECONCILE_CM_CLASS, recnData);
        return;
    }

    if (changes)
    {
        info = gnc_gui_get_entity_events (changes, &recnData->account);
        if (info && (info->event_mask & QOF_EVENT_DESTROY))
        {
            gnc_close_gui_component_by_data (WINDOW_RECONCILE_CM_CLASS, recnData);
            return;
        }
    }

    gnc_reconcile_window_set_titles (recnData);
    recn_set_watches (recnData);

    recnRefresh (recnData);
}


static void
close_handler (gpointer user_data)
{
    RecnWindow2 *recnData = user_data;

    gnc_save_window_size (GNC_PREFS_GROUP_RECONCILE, GTK_WINDOW (recnData->window));
    gtk_widget_destroy (recnData->window);
}


/********************************************************************\
 * recnWindow2                                                       *
 *   opens up the window to reconcile an account                    *
 *                                                                  *
 * Args:   parent  - the parent of this window                      *
 *         account - the account to reconcile                       *
 * Return: recnData - the instance of this RecnWindow2               *
\********************************************************************/
RecnWindow2 *
recnWindow2 (GtkWidget *parent, Account *account)
{
    gnc_numeric new_ending;
    time64 statement_date;

    if (account == NULL)
        return NULL;

    /* The last time reconciliation was attempted during the current
     * execution of gnucash, the date was stored. Use that date if
     * possible. This helps with balancing multiple accounts for which
     * statements are issued at the same time, like multiple bank
     * accounts on a single statement. */
    if (!gnc_reconcile_last_statement_date)
        statement_date = gnc_time (NULL);
    else
        statement_date = gnc_reconcile_last_statement_date;

    gnc_get_reconcile_info (account, &new_ending, &statement_date);

    /* Popup a little window to prompt the user to enter the
     * ending balance for his/her bank statement */
    if (!startRecnWindow (parent, account, &new_ending, &statement_date, TRUE))
        return NULL;

    return recnWindow2WithBalance (parent, account, new_ending, statement_date);
}


static void
recnWindow2_add_widget (GtkUIManager *merge,
                       GtkWidget *widget,
                       GtkBox *dock)
{
    gtk_box_pack_start (GTK_BOX (dock), widget, FALSE, FALSE, 0);
    gtk_widget_show (widget);
}


/********************************************************************\
 * recnWindow2WithBalance
 *
 *   Opens up the window to reconcile an account, but with ending
 *   balance and statement date already given.
 *
 * Args:   parent         - The parent widget of the new window
 *         account        - The account to reconcile
 *         new_ending     - The amount for ending balance
 *         statement_date - The date of the statement
 * Return: recnData - the instance of this RecnWindow2
\********************************************************************/
RecnWindow2 *
recnWindow2WithBalance (GtkWidget *parent, Account *account,
                       gnc_numeric new_ending, time64 statement_date)
{
    RecnWindow2 *recnData;
    GtkWidget *statusbar;
    GtkWidget *vbox;
    GtkWidget *dock;

    if (account == NULL)
        return NULL;

    recnData = gnc_find_first_gui_component (WINDOW_RECONCILE_CM_CLASS,
               find_by_account, account);
    if (recnData)
        return recnData;

    recnData = g_new0 (RecnWindow2, 1);

    recnData->account = *xaccAccountGetGUID (account);


    recnData->component_id =
        gnc_register_gui_component (WINDOW_RECONCILE_CM_CLASS,
                                    refresh_handler, close_handler,
                                    recnData);

    recn_set_watches (recnData);

    gnc_reconcile_last_statement_date = statement_date;

    recnData->new_ending = new_ending;
    recnData->statement_date = statement_date;
    recnData->window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    recnData->delete_refresh = FALSE;

    gnc_recn_set_window_name (recnData);

    vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
    gtk_box_set_homogeneous (GTK_BOX (vbox), FALSE);
    gtk_container_add (GTK_CONTAINER(recnData->window), vbox);

    dock = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
    gtk_box_set_homogeneous (GTK_BOX (dock), FALSE);
    gtk_widget_show (dock);
    gtk_box_pack_start (GTK_BOX (vbox), dock, FALSE, TRUE, 0);

    {
        gchar *filename;
        gint merge_id;
        GtkAction *action;
        GtkActionGroup *action_group;
        GError *error = NULL;

        recnData->ui_merge = gtk_ui_manager_new ();
        g_signal_connect (recnData->ui_merge, "add_widget",
                          G_CALLBACK (recnWindow2_add_widget), dock);

        action_group = gtk_action_group_new ("ReconcileWindowActions");
        recnData->action_group = action_group;
        gtk_action_group_set_translation_domain (action_group, PROJECT_NAME);
        gtk_action_group_add_actions (action_group, recnWindow2_actions,
                                      recnWindow2_n_actions, recnData);
        action =
            gtk_action_group_get_action (action_group, "AccountOpenAccountAction");
        g_object_set (G_OBJECT (action), "short_label", _("Open"), NULL);

        gtk_ui_manager_insert_action_group (recnData->ui_merge, action_group, 0);

        filename = gnc_filepath_locate_ui_file("gnc-reconcile-window-ui.xml");
        /* Can't do much without a ui. */
        g_assert (filename);

        merge_id = gtk_ui_manager_add_ui_from_file (recnData->ui_merge,
                   filename, &error);
        g_assert(merge_id || error);
        if (merge_id)
        {
            gtk_window_add_accel_group (GTK_WINDOW (recnData->window),
                                        gtk_ui_manager_get_accel_group (recnData->ui_merge));
            gtk_ui_manager_ensure_update (recnData->ui_merge);
        }
        else
        {
            g_critical("Failed to load ui file.\n  Filename %s\n  Error %s",
                       filename, error->message);
            g_error_free (error);
            g_assert (merge_id != 0);
        }
        g_free (filename);
    }

    g_signal_connect (recnData->window, "popup-menu",
                     G_CALLBACK (gnc_reconcile_window_popup_menu_cb), recnData);

    statusbar = gtk_statusbar_new ();
    gtk_box_pack_end (GTK_BOX (vbox), statusbar, FALSE, FALSE, 0);

    g_signal_connect (recnData->window, "destroy",
                      G_CALLBACK (recn_destroy_cb), recnData);
    g_signal_connect (recnData->window, "delete_event",
                      G_CALLBACK (recn_delete_cb), recnData);
    g_signal_connect (recnData->window, "key_press_event",
                      G_CALLBACK (recn_key_press_cb), recnData);

    /* The main area */
    {
        GtkWidget *frame = gtk_frame_new (NULL);
        GtkWidget *main_area = gtk_box_new (GTK_ORIENTATION_VERTICAL, 10);
        GtkWidget *debcred_area = gtk_grid_new ();
        GtkWidget *debits_box;
        GtkWidget *credits_box;

        gtk_box_set_homogeneous (GTK_BOX (main_area), FALSE);
        gtk_box_pack_start (GTK_BOX (vbox), frame, TRUE, TRUE, 10);

        /* Force a reasonable starting size */
        gtk_window_set_default_size (GTK_WINDOW (recnData->window), 800, 600);
        gnc_restore_window_size (GNC_PREFS_GROUP_RECONCILE,
                                 GTK_WINDOW (recnData->window), GTK_WINDOW(parent));

        gtk_container_add(GTK_CONTAINER(frame), main_area);
        gtk_container_set_border_width(GTK_CONTAINER(main_area), 10);

        debits_box = gnc_reconcile_window_create_view_box
                     (account, RECLIST_DEBIT, recnData,
                      &recnData->debit, &recnData->total_debit);

        credits_box = gnc_reconcile_window_create_view_box
                      (account, RECLIST_CREDIT, recnData,
                       &recnData->credit, &recnData->total_credit);

        GNC_RECONCILE_VIEW (recnData->debit)->sibling = GNC_RECONCILE_VIEW (recnData->credit);
        GNC_RECONCILE_VIEW (recnData->credit)->sibling = GNC_RECONCILE_VIEW (recnData->debit);

        gtk_box_pack_start (GTK_BOX (main_area), debcred_area, TRUE, TRUE, 0);

        gtk_grid_set_column_homogeneous (GTK_GRID(debcred_area), TRUE);
        gtk_grid_set_column_spacing (GTK_GRID(debcred_area), 15);
        gtk_grid_attach (GTK_GRID(debcred_area), debits_box, 0, 0, 1, 1);
        gtk_widget_set_hexpand (debits_box, TRUE);
        gtk_widget_set_vexpand (debits_box, TRUE);
        gtk_widget_set_halign (debits_box, GTK_ALIGN_FILL);
        gtk_widget_set_valign (debits_box, GTK_ALIGN_FILL);

        gtk_grid_attach (GTK_GRID(debcred_area), credits_box, 1, 0, 1, 1);
        gtk_widget_set_hexpand (credits_box, TRUE);
        gtk_widget_set_vexpand (credits_box, TRUE);
        gtk_widget_set_halign (credits_box, GTK_ALIGN_FILL);
        gtk_widget_set_valign (credits_box, GTK_ALIGN_FILL);

        {
            GtkWidget *hbox, *title_vbox, *value_vbox;
            GtkWidget *totals_hbox, *frame, *title, *value;

            /* lower horizontal bar below reconcile lists */
            hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 5);
            gtk_box_set_homogeneous (GTK_BOX (hbox), FALSE);
            gtk_box_pack_start (GTK_BOX (main_area), hbox, FALSE, FALSE, 0);

            /* frame to hold totals */
            frame = gtk_frame_new (NULL);
            gtk_box_pack_end (GTK_BOX (hbox), frame, FALSE, FALSE, 0);

            /* hbox to hold title/value vboxes */
            totals_hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 3);
            gtk_box_set_homogeneous (GTK_BOX (totals_hbox), FALSE);
            gtk_container_add (GTK_CONTAINER (frame), totals_hbox);
            gtk_container_set_border_width (GTK_CONTAINER (totals_hbox), 5);

            /* vbox to hold titles */
            title_vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 3);
            gtk_box_set_homogeneous (GTK_BOX (title_vbox), FALSE);
            gtk_box_pack_start (GTK_BOX (totals_hbox), title_vbox, FALSE, FALSE, 0);

            /* vbox to hold values */
            value_vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 3);
            gtk_box_set_homogeneous (GTK_BOX (value_vbox), FALSE);
            gtk_box_pack_start (GTK_BOX (totals_hbox), value_vbox, TRUE, TRUE, 0);

            /* statement date title/value */
            title = gtk_label_new (_("Statement Date"));
            gnc_label_set_alignment (title, 1.0, 0.5);
            gtk_box_pack_start (GTK_BOX (title_vbox), title, FALSE, FALSE, 0);

            value = gtk_label_new ("");
            recnData->recn_date = value;
            gnc_label_set_alignment (value, 1.0, 0.5);
            gtk_box_pack_start (GTK_BOX (value_vbox), value, FALSE, FALSE, 0);

            /* starting balance title/value */
            title = gtk_label_new(_("Starting Balance"));
            gnc_label_set_alignment (title, 1.0, 0.5);
            gtk_box_pack_start (GTK_BOX (title_vbox), title, FALSE, FALSE, 3);

            value = gtk_label_new ("");
            recnData->starting = value;
            gnc_label_set_alignment (value, 1.0, 0.5);
            gtk_box_pack_start (GTK_BOX (value_vbox), value, FALSE, FALSE, 3);

            /* ending balance title/value */
            title = gtk_label_new (_("Ending Balance"));
            gnc_label_set_alignment (title, 1.0, 0.5);
            gtk_box_pack_start (GTK_BOX (title_vbox), title, FALSE, FALSE, 0);

            value = gtk_label_new ("");
            recnData->ending = value;
            gnc_label_set_alignment (value, 1.0, 0.5);
            gtk_box_pack_start (GTK_BOX (value_vbox), value, FALSE, FALSE, 0);

            /* reconciled balance title/value */
            title = gtk_label_new (_("Reconciled Balance"));
            gnc_label_set_alignment (title, 1.0, 0.5);
            gtk_box_pack_start (GTK_BOX (title_vbox), title, FALSE, FALSE, 0);

            value = gtk_label_new ("");
            recnData->reconciled = value;
            gnc_label_set_alignment (value, 1.0, 0.5);
            gtk_box_pack_start (GTK_BOX (value_vbox), value, FALSE, FALSE, 0);

            /* difference title/value */
            title = gtk_label_new (_("Difference"));
            gnc_label_set_alignment (title, 1.0, 0.5);
            gtk_box_pack_start (GTK_BOX (title_vbox), title, FALSE, FALSE, 0);

            value = gtk_label_new ("");
            recnData->difference = value;
            gnc_label_set_alignment (value, 1.0, 0.5);
            gtk_box_pack_start (GTK_BOX (value_vbox), value, FALSE, FALSE, 0);
        }

        /* Set up the data */
        recnRefresh (recnData);
    }

    /* Allow resize */
    gtk_window_set_resizable (GTK_WINDOW (recnData->window), TRUE);

    gtk_widget_show_all (recnData->window);

    gnc_reconcile_window_set_titles (recnData);

    recnRecalculateBalance (recnData);

    gnc_window_adjust_for_screen (GTK_WINDOW (recnData->window));

    /* Set the sort orders of the debit and credit tree views */
    gnc_query_sort_order (GNC_QUERY_VIEW (recnData->debit), REC_DATE, GTK_SORT_ASCENDING);
    gnc_query_sort_order (GNC_QUERY_VIEW (recnData->credit), REC_DATE, GTK_SORT_ASCENDING);

    gtk_widget_grab_focus (recnData->debit);

    return recnData;
}


/********************************************************************\
 * gnc_ui_reconile_window2_raise                                     *
 *   shows and raises an account editing window                     *
 *                                                                  *
 * Args:   editAccData - the edit window structure                  *
\********************************************************************/
void
gnc_ui_reconcile_window2_raise (RecnWindow2 * recnData)
{
    if (recnData == NULL)
        return;

    if (recnData->window == NULL)
        return;

    gtk_window_present (GTK_WINDOW (recnData->window));
}


/********************************************************************\
 * recn_destroy_cb                                                  *
 *   frees memory allocated for an recnWindow2, and other cleanup    *
 *   stuff                                                          *
 *                                                                  *
 * Args:   w    - the widget that called us                         *
 *         data - the data struct for this window                   *
 * Return: none                                                     *
\********************************************************************/
static void
recn_destroy_cb (GtkWidget *w, gpointer data)
{
    RecnWindow2 *recnData = data;

    gnc_unregister_gui_component_by_data (WINDOW_RECONCILE_CM_CLASS, recnData);

    if (recnData->delete_refresh)
        gnc_resume_gui_refresh ();

    g_free (recnData);
}


static void
recn_cancel (RecnWindow2 *recnData)
{
    gboolean changed = FALSE;

    if (gnc_reconcile_view_changed (GNC_RECONCILE_VIEW (recnData->credit)))
        changed = TRUE;
    if (gnc_reconcile_view_changed (GNC_RECONCILE_VIEW (recnData->debit)))
        changed = TRUE;

    if (changed)
    {
        const char *message = _("You have made changes to this reconcile "
                                "window. Are you sure you want to cancel?");
        if (!gnc_verify_dialog (GTK_WINDOW (recnData->window), FALSE, "%s", message))
            return;
    }

    gnc_close_gui_component_by_data (WINDOW_RECONCILE_CM_CLASS, recnData);
}


static gboolean
recn_delete_cb (GtkWidget *widget, GdkEvent *event, gpointer data)
{
    RecnWindow2 *recnData = data;

    recn_cancel (recnData);
    return TRUE;
}


static gboolean
recn_key_press_cb (GtkWidget *widget, GdkEventKey *event, gpointer data)
{
    RecnWindow2 *recnData = data;

    if (event->keyval == GDK_KEY_Escape)
    {
        recn_cancel (recnData);
        return TRUE;
    }
    else
    {
        return FALSE;
    }
}


/********************************************************************\
 * find_payment_account                                             *
 *   find an account that 'looks like' a payment account for the    *
 *   given account. This really only makes sense for credit card    *
 *   accounts.                                                      *
 *                                                                  *
 * Args:   account - the account to look in                         *
 * Return: a candidate payment account or NULL if none was found    *
\********************************************************************/
static Account *
find_payment_account (Account *account)
{
    GList *list;
    GList *node;

    if (account == NULL)
        return NULL;

    list = xaccAccountGetSplitList (account);

    /* Search backwards to find the latest payment */
    for (node = g_list_last (list); node; node = node->prev)
    {
        Transaction *trans;
        Split *split;
        GList *n;

        split = node->data;
        if (split == NULL)
            continue;

        /* ignore 'purchases' */
        if (!gnc_numeric_positive_p (xaccSplitGetAmount (split)))
            continue;

        trans = xaccSplitGetParent(split);
        if (trans == NULL)
            continue;

        for (n = xaccTransGetSplitList (trans); n; n = n->next)
        {
            GNCAccountType type;
            Account *a;
            Split *s;

            s = n->data;
            if ((s == NULL) || (s == split))
                continue;

            a = xaccSplitGetAccount(s);
            if ((a == NULL) || (a == account))
                continue;

            type = xaccAccountGetType(a);
            if ((type == ACCT_TYPE_BANK) || (type == ACCT_TYPE_CASH) ||
                    (type == ACCT_TYPE_ASSET))
                return a;
        }
    }

    return NULL;
}


/********************************************************************\
 * recnFinishCB                                                     *
 *   saves reconcile information                                    *
 *                                                                  *
 * Args:   w    - the widget that called us                         *
 *         data - the data struct for this window                   *
 * Return: none                                                     *
\********************************************************************/
static void
recnFinishCB (GtkAction *action, RecnWindow2 *recnData)
{
    gboolean auto_payment;
    Account *account;
    time64 date;

    if (!gnc_numeric_zero_p (recnRecalculateBalance (recnData)))
    {
        const char *message = _("The account is not balanced. "
                                "Are you sure you want to finish?");
        if (!gnc_verify_dialog (GTK_WINDOW (recnData->window), FALSE, "%s", message))
            return;
    }

    date = recnData->statement_date;

    gnc_suspend_gui_refresh ();

    recnData->delete_refresh = TRUE;

    gnc_reconcile_view_commit (GNC_RECONCILE_VIEW (recnData->credit), date);
    gnc_reconcile_view_commit (GNC_RECONCILE_VIEW (recnData->debit), date);

    auto_payment = gnc_prefs_get_bool (GNC_PREFS_GROUP_RECONCILE, GNC_PREF_AUTO_CC_PAYMENT);

    account = recn_get_account (recnData);

    xaccAccountClearReconcilePostpone (account);
    xaccAccountSetReconcileLastDate (account, date);

    if (auto_payment &&
            (xaccAccountGetType (account) == ACCT_TYPE_CREDIT) &&
            (gnc_numeric_negative_p (recnData->new_ending)))
    {
        Account *payment_account;
        XferDialog *xfer;

        xfer = gnc_xfer_dialog (GTK_WIDGET (recnData->window), account);

        gnc_xfer_dialog_set_amount (xfer, gnc_numeric_neg (recnData->new_ending));

        payment_account = find_payment_account (account);
        if (payment_account != NULL)
            gnc_xfer_dialog_select_from_account (xfer, payment_account);
    }

    gnc_close_gui_component_by_data (WINDOW_RECONCILE_CM_CLASS, recnData);
}


/********************************************************************\
 * recnPostponeCB                                                   *
 *   saves reconcile information for later use                      *
 *                                                                  *
 * Args:   w    - the widget that called us                         *
 *         data - the data struct for this window                   *
 * Return: none                                                     *
\********************************************************************/
static void
recnPostponeCB (GtkAction *action, gpointer data)
{
    RecnWindow2 *recnData = data;
    Account *account;

    {
        const char *message = _("Do you want to postpone this reconciliation "
                                "and finish it later?");
        if (!gnc_verify_dialog (GTK_WINDOW (recnData->window), FALSE, "%s", message))
            return;
    }

    gnc_suspend_gui_refresh ();

    recnData->delete_refresh = TRUE;

    gnc_reconcile_view_postpone (GNC_RECONCILE_VIEW (recnData->credit));
    gnc_reconcile_view_postpone (GNC_RECONCILE_VIEW (recnData->debit));

    account = recn_get_account (recnData);

    xaccAccountSetReconcilePostponeDate (account, recnData->statement_date);
    xaccAccountSetReconcilePostponeBalance (account, recnData->new_ending);

    gnc_close_gui_component_by_data (WINDOW_RECONCILE_CM_CLASS, recnData);
}


static void
recnCancelCB (GtkAction *action, gpointer data)
{
    RecnWindow2 *recnData = data;
    recn_cancel (recnData);
}


/** An array of all of the actions provided by the main window code.
 *  This includes some placeholder actions for the menus that are
 *  visible in the menu bar but have no action associated with
 *  them. */
static GtkActionEntry recnWindow2_actions [] =
{
    /* Toplevel */

    { "ReconcileMenuAction",   NULL, N_("_Reconcile"), NULL, NULL, NULL, },
    { "AccountMenuAction",     NULL, N_("_Account"), NULL, NULL, NULL, },
    { "TransactionMenuAction", NULL, N_("_Transaction"), NULL, NULL, NULL, },
    { "HelpMenuAction",        NULL, N_("_Help"), NULL, NULL, NULL, },

    /* Reconcile menu */

    {
        "RecnChangeInfoAction", NULL, N_("_Reconcile Information..."),  NULL,
        N_("Change the reconcile information "
        "including statement date and ending balance."),
        G_CALLBACK (gnc_ui_reconcile_window_change_cb)
    },
    {
        "RecnFinishAction", "system-run", N_("_Finish"), "<primary>w",
        N_("Finish the reconciliation of this account"),
        G_CALLBACK(recnFinishCB)
    },
    {
        "RecnPostponeAction", "go-previous", N_("_Postpone"), "<primary>p",
        N_("Postpone the reconciliation of this account"),
        G_CALLBACK(recnPostponeCB)
    },
    {
        "RecnCancelAction", "process-stop", N_("_Cancel"), NULL,
        N_("Cancel the reconciliation of this account"),
        G_CALLBACK(recnCancelCB)
    },

    /* Account menu */

    {
        "AccountOpenAccountAction", "go-jump", N_("_Open Account"), NULL,
        N_("Open the account"),
        G_CALLBACK(gnc_recn_open_cb)
    },
    {
        "AccountEditAccountAction", NULL, N_("_Edit Account"), NULL,
        N_("Edit the main account for this register"),
        G_CALLBACK(gnc_recn_edit_account_cb)
    },
    {
        "AccountTransferAction", NULL, N_("_Transfer..."), NULL,
        N_("Transfer funds from one account to another"),
        G_CALLBACK(gnc_recn_xfer_cb)
    },
    {
        "AccountCheckRepairAction", NULL, N_("_Check & Repair"), NULL,
        N_("Check for and repair unbalanced transactions and orphan splits "
        "in this account"),
        G_CALLBACK(gnc_recn_scrub_cb)
    },

    /* Transaction menu */

    {
        "TransBalanceAction", "document-new", N_("_Balance"), "<primary>b",
        N_("Add a new balancing entry to the account"),
        G_CALLBACK(gnc_ui_reconcile_window_balance_cb)
    },
    {
        "TransEditAction", "document-properties", N_("_Edit"),  "<primary>e",
        N_("Edit the current transaction"),
        G_CALLBACK(gnc_ui_reconcile_window_edit_cb)
    },
    {
        "TransDeleteAction", "edit-delete", N_("_Delete"),  "<primary>d",
        N_("Delete the selected transaction"),
        G_CALLBACK(gnc_ui_reconcile_window_delete_cb)
    },
    {
        "TransRecAction", "emblem-default", N_("_Reconcile Selection"), "<primary>r",
        N_("Reconcile the selected transactions"),
        G_CALLBACK(gnc_ui_reconcile_window_rec_cb)
    },
    {
        "TransUnRecAction", "edit-clear", N_("_Unreconcile Selection"), "<primary>u",
        N_("Unreconcile the selected transactions"),
        G_CALLBACK(gnc_ui_reconcile_window_unrec_cb)
    },

    /* Help menu */

    {
        "HelpHelpAction", NULL, N_("_Help"), NULL,
        N_("Open the GnuCash help window"),
        G_CALLBACK(gnc_ui_reconcile_window_help_cb)
    },
};

/** The number of actions provided by the main window. */
static guint recnWindow2_n_actions = G_N_ELEMENTS (recnWindow2_actions);
