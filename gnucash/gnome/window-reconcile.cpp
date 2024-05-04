/********************************************************************\
 * window-reconcile.c -- the reconcile window                       *
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

#include "Account.hpp"
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
#include "gnc-gtk-utils.h"
//#include "gnc-main-window.h"
#include "gnc-plugin-page-register.h"
#include "gnc-prefs.h"
#include "gnc-ui.h"
#include "gnc-ui-balances.h"
#include "gnc-window.h"
#include "reconcile-view.h"
#include "window-reconcile.h"
#include "gnc-session.h"
#ifdef MAC_INTEGRATION
#include <gtkmacintegration/gtkosxapplication.h>
#endif

#define WINDOW_RECONCILE_CM_CLASS "window-reconcile"
#define GNC_PREF_AUTO_CC_PAYMENT        "auto-cc-payment"
#define GNC_PREF_ALWAYS_REC_TO_TODAY    "always-reconcile-to-today"


/** STRUCTS *********************************************************/
struct _RecnWindow
{
    GncGUID account;             /* The account that we are reconciling  */
    gnc_numeric new_ending;      /* The new ending balance               */
    time64 statement_date;       /* The statement date                   */

    gint component_id;           /* id of component                      */

    GtkWidget *window;           /* The reconcile window                 */

    GtkBuilder *builder;         /* The builder object */
    GSimpleActionGroup *simple_action_group; /* The action group for the window */

    GncPluginPage *page;

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
    GtkWidget     *future_icon;
    GtkWidget     *future_text;
    GNCAmountEdit *end_value;       /* the dialog's ending balance amount edit */
    gnc_numeric    original_value;  /* the dialog's original ending balance    */
    gboolean       user_set_value;  /* the user changed the ending value       */

    XferDialog    *xferData;        /* the interest xfer dialog (if it exists) */
    gboolean       include_children;

    time64         date;            /* the interest xfer reconcile date        */
} startRecnWindowData;

/** PROTOTYPES ******************************************************/
static gnc_numeric recnRecalculateBalance (RecnWindow *recnData);

static void   recn_destroy_cb (GtkWidget *w, gpointer data);
static void   recn_cancel (RecnWindow *recnData);
static gboolean recn_delete_cb (GtkWidget *widget, const GdkEvent *event, gpointer data);
static gboolean recn_key_press_cb (GtkEventControllerKey *key, guint keyval,
                                   guint keycode, GdkModifierType state,
                                   gpointer user_data);
static void   recnFinishCB (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void   recnPostponeCB (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void   recnCancelCB (GSimpleAction *simple, GVariant *parameter, gpointer user_data);

extern "C" {
void gnc_start_recn_children_changed (GtkWidget *widget, startRecnWindowData *data);
void gnc_start_recn_interest_clicked_cb (GtkButton *button, startRecnWindowData *data);
}

static void   gnc_reconcile_window_set_sensitivity (RecnWindow *recnData);
static char * gnc_recn_make_window_name (Account *account);
static void   gnc_recn_set_window_name (RecnWindow *recnData);
static gboolean find_by_account (gpointer find_data, gpointer user_data);

/** GLOBALS ************************************************************/
/* This static indicates the debugging module that this .o belongs to. */
G_GNUC_UNUSED static QofLogModule log_module = GNC_MOD_GUI;

static time64 gnc_reconcile_last_statement_date = 0;

/** IMPLEMENTATIONS *************************************************/

static gpointer
commodity_compare(Account *account, gpointer user_data) {
  gboolean equal = gnc_commodity_equiv (xaccAccountGetCommodity (account),
                                        (gnc_commodity*) user_data);

  return equal ? NULL : account;
}


/********************************************************************\
 * has_account_different_commodities                                *
 *                                                                  *
 * Args:   parent account - the account to look in                  *
 * Return: true if there exists a subaccount with different         *
 *    commodity then the parent account.                            *
\********************************************************************/
static gboolean
has_account_different_commodities(const Account *account)
{
    gnc_commodity *parent_commodity;
    gpointer result;

    if (account == NULL)
        return FALSE;

    parent_commodity = xaccAccountGetCommodity (account);

    result = gnc_account_foreach_descendant_until (account,
                                                   commodity_compare,
                                                   parent_commodity);

    return result != NULL;
}


/********************************************************************\
 * recnRefresh                                                      *
 *   refreshes the transactions in the reconcile window             *
 *                                                                  *
 * Args:   account - the account of the reconcile window to refresh *
 * Return: none                                                     *
\********************************************************************/
static void
recnRefresh (RecnWindow *recnData)
{
    if (recnData == NULL)
        return;

    gnc_reconcile_view_refresh(GNC_RECONCILE_VIEW(recnData->debit));
    gnc_reconcile_view_refresh(GNC_RECONCILE_VIEW(recnData->credit));

    gnc_reconcile_window_set_sensitivity(recnData);

    gnc_recn_set_window_name(recnData);

    recnRecalculateBalance(recnData);

    gtk_widget_queue_resize(recnData->window);
}


static Account *
recn_get_account (RecnWindow *recnData)
{
    if (!recnData)
        return NULL;

    return xaccAccountLookup (&recnData->account, gnc_get_current_book ());
}


static void
gnc_add_colorized_amount (gpointer obj, gnc_numeric amt,
                          GNCPrintAmountInfo print_info, gboolean reverse)
{
    if (!obj) return;
    if (reverse) amt = gnc_numeric_neg (amt);
    gnc_set_label_color (GTK_WIDGET (obj), amt);
    gtk_label_set_text (GTK_LABEL (obj), xaccPrintAmount (amt, print_info));
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
recnRecalculateBalance (RecnWindow *recnData)
{
    Account *account;
    gnc_numeric debit;
    gnc_numeric credit;
    gnc_numeric starting;
    gnc_numeric ending;
    gnc_numeric reconciled;
    gnc_numeric diff;
    gchar *datestr;
    GNCPrintAmountInfo print_info;
    gboolean reverse_balance, include_children;
    GAction *action;

    account = recn_get_account (recnData);
    if (!account)
        return gnc_numeric_zero ();

    reverse_balance = gnc_reverse_balance(account);
    include_children = xaccAccountGetReconcileChildrenStatus(account);
    starting = gnc_ui_account_get_reconciled_balance(account, include_children);
    print_info = gnc_account_print_info (account, TRUE);

    ending = recnData->new_ending;
    debit = gnc_reconcile_view_reconciled_balance
            (GNC_RECONCILE_VIEW(recnData->debit));
    credit = gnc_reconcile_view_reconciled_balance
             (GNC_RECONCILE_VIEW(recnData->credit));

    reconciled = gnc_numeric_sub_fixed (debit, credit);
    if (reverse_balance)
        reconciled = gnc_numeric_sub_fixed (reconciled, starting);
    else
        reconciled = gnc_numeric_add_fixed (reconciled, starting);

    diff = gnc_numeric_sub_fixed (ending, reconciled);

    datestr = qof_print_date (recnData->statement_date);
    gtk_label_set_text (GTK_LABEL(recnData->recn_date), datestr);
    g_free (datestr);

    gnc_add_colorized_amount (recnData->starting, starting, print_info, FALSE);
    gnc_add_colorized_amount (recnData->ending, ending, print_info, reverse_balance);
    gnc_add_colorized_amount (recnData->total_debit, debit, print_info, FALSE);
    gnc_add_colorized_amount (recnData->total_credit, credit, print_info, FALSE);
    gnc_add_colorized_amount (recnData->reconciled, reconciled, print_info, reverse_balance);
    gnc_add_colorized_amount (recnData->difference, diff, print_info, reverse_balance);

    action = g_action_map_lookup_action (G_ACTION_MAP(recnData->simple_action_group),
                                         "RecnFinishAction");
    g_simple_action_set_enabled (G_SIMPLE_ACTION(action), gnc_numeric_zero_p (diff));

    action = g_action_map_lookup_action (G_ACTION_MAP(recnData->simple_action_group),
                                         "TransBalanceAction");
    g_simple_action_set_enabled (G_SIMPLE_ACTION(action), !gnc_numeric_zero_p (diff));

    return diff;
}


/* amount_edit_focus_out_cb
 *   Callback on focus-out event for statement Ending Balance.
 *   Sets the user_set_value flag true if the amount entered is
 *   different to the calculated Ending Balance as at the default
 *   Statement Date. This prevents the entered Ending Balance
 *   being recalculated if the Statement Date is changed.
 *
 * Args:   widget         - Ending Balance widget
 *         event          - event triggering this callback
 *         data           - structure containing info about this
 *                          reconciliation process.
 * Returns:  False - propagate the event to the widget's parent.
 */
static gboolean
amount_edit_focus_out_cb (GtkEventControllerFocus *controller,
                          gpointer user_data)
{
    startRecnWindowData *srwd = (startRecnWindowData*)user_data;
    gnc_numeric value;
    gint result = gnc_amount_edit_expr_is_valid (GNC_AMOUNT_EDIT(srwd->end_value),
                                                 &value, TRUE, NULL);

    srwd->user_set_value = FALSE;

    if (result < 1) // OK
    {
        if (result == -1) // blank entry is valid
        {
            gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT(srwd->end_value), value);
            gnc_amount_edit_select_region (GNC_AMOUNT_EDIT(srwd->end_value), 0, -1);
        }
        srwd->user_set_value = !gnc_numeric_equal (value, srwd->original_value);
    }
    return FALSE;
}


/* recn_date_changed_cb
 *   Callback on date_changed event for Statement Date.
 *   If the user changed the date edit widget, and the Ending
 *   Balance wasn't entered, update the Ending Balance to reflect
 *   the ending balance of the account as at Statement Date.
 *
 * Args:   widget         - Statement Date edit widget
 *         data           - structure containing info about this
 *                          reconciliation.
 * Returns:  none.
 */
static void
recn_date_changed_cb (GtkWidget *widget, startRecnWindowData *data)
{
    GNCDateEdit *gde = GNC_DATE_EDIT (widget);
    gnc_numeric new_balance;
    time64 new_date;

    gboolean show_warning = FALSE;
    gint days_after_today;
    static const time64 secs_per_day = 86400;
    static const time64 secs_per_hour = 3600;

    new_date = gnc_date_edit_get_date_end (gde);

    /* Add secs_per_hour to the difference to compensate for the short
     * day when transitioning from standard to daylight time.
     */
    days_after_today = (gnc_time64_get_day_end (new_date) -
                        gnc_time64_get_today_end () +
                        secs_per_hour) / secs_per_day;

    if (days_after_today > 0)
    {
        gchar *str = g_strdup_printf
            /* Translators: %d is the number of days in the future */
            (ngettext ("Statement Date is %d day after today.",
                       "Statement Date is %d days after today.",
                       days_after_today),
             days_after_today);

        gchar *tip_start = g_strdup_printf
            /* Translators: %d is the number of days in the future */
            (ngettext ("The statement date you have chosen is %d day in the future.",
                       "The statement date you have chosen is %d days in the future.",
                       days_after_today),
             days_after_today);

        gchar *tip_end = g_strdup (_("This may cause issues for future reconciliation \
actions on this account. Please double-check this is the date you intended."));
        gchar *tip = g_strdup_printf ("%s %s", tip_start, tip_end);

        show_warning = TRUE;

        gtk_label_set_text (GTK_LABEL(data->future_text), str);
        gtk_widget_set_tooltip_text (GTK_WIDGET(data->future_text), tip);
        g_free (str);
        g_free (tip_end);
        g_free (tip_start);
        g_free (tip);
    }
    gtk_widget_set_visible (GTK_WIDGET(data->future_icon), show_warning);
    gtk_widget_set_visible (GTK_WIDGET(data->future_text), show_warning);

    if (data->user_set_value)
        return;

    /* get the balance for the account as of the new date */
    new_balance = gnc_ui_account_get_balance_as_of_date (data->account, new_date,
                  data->include_children);
    /* update the amount edit with the amount */
    gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (data->end_value),
                                new_balance);
}


void
gnc_start_recn_children_changed (GtkWidget *widget, startRecnWindowData *data)
{
    data->include_children =
        gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget));

    /* Force an update of the ending balance */
    recn_date_changed_cb (data->date_value, data);
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
gnc_recn_make_interest_window_name(Account *account, char *text)
{
    char *fullname;
    char *title;

    fullname = gnc_account_get_full_name(account);
    title = g_strconcat(fullname, " - ", text && *text ? _(text) : "", NULL);

    g_free(fullname);

    return title;
}


static void
recnInterestXferWindow( startRecnWindowData *data)
{
    gchar *title;

    if ( !account_type_has_auto_interest_xfer( data->account_type ) )
        return;

    /* get a normal transfer dialog... */
    data->xferData = gnc_xfer_dialog( GTK_WIDGET(data->startRecnWindow),
                                      data->account );

    /* ...and start changing things: */

    /* change title */
    if ( account_type_has_auto_interest_payment( data->account_type ) )
        title = gnc_recn_make_interest_window_name( data->account,
                _("Interest Payment") );
    else
        title = gnc_recn_make_interest_window_name( data->account,
                _("Interest Charge") );

    gnc_xfer_dialog_set_title( data->xferData, title );
    g_free( title );


    /* change frame labels */
    gnc_xfer_dialog_set_information_label( data->xferData,
                                           _("Payment Information") );

    /* Interest accrued is a transaction from an income account
     * to a bank account.  Interest charged is a transaction from
     * a credit account to an expense account.  The user isn't allowed
     * to change the account (bank or credit) being reconciled.
     */
    if ( account_type_has_auto_interest_payment( data->account_type ) )
    {
        gnc_xfer_dialog_set_from_account_label( data->xferData,
                                                _("Payment From") );
        gnc_xfer_dialog_set_from_show_button_active( data->xferData, TRUE );

        // XXX: Set "from" account from previous interest payment.

        gnc_xfer_dialog_set_to_account_label( data->xferData,
                                              _("Reconcile Account") );
        gnc_xfer_dialog_select_to_account( data->xferData, data->account );
        gnc_xfer_dialog_lock_to_account_tree( data->xferData );

        /* Quickfill based on the reconcile account, which is the "To" acct. */
        gnc_xfer_dialog_quickfill_to_account( data->xferData, TRUE );
    }
    else  /* interest charged to account rather than paid to it */
    {
        gnc_xfer_dialog_set_from_account_label( data->xferData,
                                                _("Reconcile Account") );
        gnc_xfer_dialog_select_from_account( data->xferData, data->account );
        gnc_xfer_dialog_lock_from_account_tree( data->xferData );

        gnc_xfer_dialog_set_to_account_label( data->xferData,
                                              _("Payment To") );
        gnc_xfer_dialog_set_to_show_button_active( data->xferData, TRUE );

        // XXX: Set "to" account from previous interest payment.

        /* Quickfill based on the reconcile account, which is the "From" acct. */
        gnc_xfer_dialog_quickfill_to_account( data->xferData, FALSE );
    }

    /* no currency frame */
    gnc_xfer_dialog_toggle_currency_table( data->xferData, FALSE );

    /* set the reconcile date for the transaction date */
    gnc_xfer_dialog_set_date( data->xferData, data->date );

    /* Now run the transfer dialog.  This blocks until done.
     * If the user hit Cancel, make the button clickable so that
     * the user can retry if they want.  We don't make the button
     * clickable if they successfully entered a transaction, since
     * the fact that the button was clickable again might make
     * the user think that the transaction didn't actually go through.
     */
    if ( ! gnc_xfer_dialog_run_until_done( data->xferData ) )
        if ( data->xfer_button )
            gtk_widget_set_sensitive(GTK_WIDGET(data->xfer_button), TRUE);

    /* done with the XferDialog */
    data->xferData = NULL;
}


/* Set up for the interest xfer window, run the window, and update
 * the startRecnWindow if the interest xfer changed anything that matters.
 */
static void
gnc_reconcile_interest_xfer_run(startRecnWindowData *data)
{
    GtkWidget *entry = gnc_amount_edit_gtk_entry(
                           GNC_AMOUNT_EDIT(data->end_value) );
    gnc_numeric before = gnc_amount_edit_get_amount(
                             GNC_AMOUNT_EDIT(data->end_value) );
    gnc_numeric after;

    recnInterestXferWindow( data );

    /* recompute the ending balance */
    after = xaccAccountGetBalanceAsOfDate(data->account, data->date);

    /* update the ending balance in the startRecnWindow if it has changed. */
    if ( gnc_numeric_compare( before, after ) )
    {
        if (gnc_reverse_balance(data->account))
            after = gnc_numeric_neg (after);

        gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (data->end_value), after);
        gtk_widget_grab_focus(GTK_WIDGET(entry));
        gtk_editable_select_region (GTK_EDITABLE(entry), 0, -1);
        data->original_value = after;
        data->user_set_value = FALSE;
    }
}


void
gnc_start_recn_interest_clicked_cb(GtkButton *button, startRecnWindowData *data)
{
    /* make the button unclickable since we're popping up the window */
    if ( data->xfer_button )
        gtk_widget_set_sensitive(GTK_WIDGET(data->xfer_button), FALSE);

    /* run the account window */
    gnc_reconcile_interest_xfer_run( data );
}


static void
gnc_save_reconcile_interval(Account *account, time64 statement_date)
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
     * value is 28 days which could be either 4 weeks or 1 month.
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
        xaccAccountSetReconcileLastInterval(account, months, days);
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
startRecnWindow(GtkWidget *parent, Account *account,
                gnc_numeric *new_ending, time64 *statement_date,
                gboolean enable_subaccount)
{
    GtkWidget *dialog, *end_value, *date_value, *include_children_button;
    GtkBuilder *builder;
    startRecnWindowData data = { NULL };
    gboolean auto_interest_xfer_option;
    GNCPrintAmountInfo print_info;
    gnc_numeric ending;
    GtkWidget *entry;
    char *title;
    int result = -6;
    gulong fo_handler_id;

    /* Initialize the data structure that will be used for several callbacks
     * throughout this file with the relevant info.  Some initialization is
     * done below as well.  Note that local storage should be OK for this,
     * since any callbacks using it will only work while the startRecnWindow
     * is running.
     */
    data.account = account;
    data.account_type = xaccAccountGetType(account);
    data.date = *statement_date;

    /* whether to have an automatic interest xfer dialog or not */
    auto_interest_xfer_option = xaccAccountGetAutoInterest (account);

    data.include_children = !has_account_different_commodities(account) &&
        xaccAccountGetReconcileChildrenStatus(account);

    ending = gnc_ui_account_get_reconciled_balance(account,
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
    gtk_builder_set_current_object (builder, G_OBJECT(&data));
    gnc_builder_add_from_file (builder, "window-reconcile.glade", "reconcile_start_dialog");

    dialog = GTK_WIDGET(gtk_builder_get_object (builder, "reconcile_start_dialog"));

    // Set the name for this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(dialog), "gnc-id-reconcile-start");

    title = gnc_recn_make_window_name (account);
    gtk_window_set_title(GTK_WINDOW(dialog), title);
    g_free (title);

    data.startRecnWindow = GTK_WIDGET(dialog);

    if (parent != NULL)
        gtk_window_set_transient_for (GTK_WINDOW (dialog), GTK_WINDOW (parent));

    {
        GtkWidget *start_value, *box;
        GtkWidget *label;
        GtkWidget *interest = NULL;

        start_value = GTK_WIDGET(gtk_builder_get_object (builder, "start_value"));
        gtk_label_set_text(GTK_LABEL(start_value), xaccPrintAmount (ending, print_info));

        include_children_button = GTK_WIDGET(gtk_builder_get_object (builder, "subaccount_check"));
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(include_children_button),
                                     data.include_children);
        gtk_widget_set_sensitive(include_children_button, enable_subaccount);

        date_value = gnc_date_edit_new(*statement_date, FALSE, FALSE);
        data.date_value = date_value;
        box = GTK_WIDGET(gtk_builder_get_object (builder, "date_value_box"));
        gtk_box_append (GTK_BOX(box), GTK_WIDGET(date_value));
        label = GTK_WIDGET(gtk_builder_get_object (builder, "date_label"));
        gnc_date_make_mnemonic_target(GNC_DATE_EDIT(date_value), label);

        end_value = gnc_amount_edit_new ();
        data.end_value = GNC_AMOUNT_EDIT(end_value);
        data.original_value = *new_ending;
        data.user_set_value = FALSE;

        data.future_icon = GTK_WIDGET(gtk_builder_get_object (builder, "future_icon"));
        data.future_text = GTK_WIDGET(gtk_builder_get_object (builder, "future_text"));

        box = GTK_WIDGET(gtk_builder_get_object (builder, "ending_value_box"));
        gtk_box_append (GTK_BOX(box), GTK_WIDGET(end_value));
        label = GTK_WIDGET(gtk_builder_get_object (builder, "end_label"));
        gnc_amount_edit_make_mnemonic_target (GNC_AMOUNT_EDIT(end_value), label);

//FIXME gtk4        gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, &data);

        gnc_date_activates_default(GNC_DATE_EDIT(date_value), TRUE);

        /* need to get a callback on date changes to update the recn balance */
        g_signal_connect ( G_OBJECT (date_value), "date_changed",
                           G_CALLBACK (recn_date_changed_cb), (gpointer) &data );

        print_info.use_symbol = 0;
        gnc_amount_edit_set_print_info (GNC_AMOUNT_EDIT (end_value), print_info);
        gnc_amount_edit_set_fraction (GNC_AMOUNT_EDIT (end_value),
                                      xaccAccountGetCommoditySCU (account));

        gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (end_value), *new_ending);

        entry = gnc_amount_edit_gtk_entry (GNC_AMOUNT_EDIT (end_value));
        gtk_editable_select_region (GTK_EDITABLE(entry), 0, -1);

        GtkEventController *event_controller = gtk_event_controller_focus_new ();
        gtk_widget_add_controller (GTK_WIDGET(entry), event_controller);
        fo_handler_id = g_signal_connect (G_OBJECT(event_controller), "leave",
                                          G_CALLBACK(amount_edit_focus_out_cb),
                                          (gpointer) &data);
        gtk_entry_set_activates_default(GTK_ENTRY(entry), TRUE);

        /* if it's possible to enter an interest payment or charge for this
         * account, add a button so that the user can pop up the appropriate
         * dialog if it isn't automatically popping up.
         */
        interest = GTK_WIDGET(gtk_builder_get_object (builder, "interest_button"));
        if ( account_type_has_auto_interest_payment( data.account_type ) )
            gtk_button_set_label(GTK_BUTTON(interest), _("Enter _Interest Payment…") );
        else if ( account_type_has_auto_interest_charge( data.account_type ) )
            gtk_button_set_label(GTK_BUTTON(interest), _("Enter _Interest Charge…") );
        else
        {
//FIXME gtk4            gtk_widget_destroy(interest);
            interest = NULL;
        }

        if ( interest )
        {
            data.xfer_button = interest;
            if ( auto_interest_xfer_option )
                gtk_widget_set_sensitive(GTK_WIDGET(interest), FALSE);
        }

//FIXME gtk4        gtk_widget_show_all(dialog);

        gtk_widget_set_visible (GTK_WIDGET(data.future_text), FALSE);
        gtk_widget_set_visible (GTK_WIDGET(data.future_icon), FALSE);

        gtk_widget_grab_focus(gnc_amount_edit_gtk_entry
                              (GNC_AMOUNT_EDIT (end_value)));
    }

    /* Allow the user to enter an interest payment
     * or charge prior to reconciling */
    if ( account_type_has_auto_interest_xfer( data.account_type )
            && auto_interest_xfer_option )
    {
        gnc_reconcile_interest_xfer_run( &data );
    }

//FIXME gtk4    while (gtk_dialog_run (GTK_DIALOG(dialog)) == GTK_RESPONSE_OK)
gtk_window_set_modal (GTK_WINDOW(dialog), TRUE); //FIXME gtk4
result = GTK_RESPONSE_CANCEL;

//    while (gtk_dialog_run (GTK_DIALOG(dialog)) == GTK_RESPONSE_OK)
//    {
        /* If response is OK but end_value not valid, try again */
//        if (gnc_amount_edit_evaluate (GNC_AMOUNT_EDIT(end_value), NULL))
//        {
//            result = GTK_RESPONSE_OK;
//            break;
//        }
//    }

    if (result == GTK_RESPONSE_OK)
    {
        *new_ending = gnc_amount_edit_get_amount (GNC_AMOUNT_EDIT (end_value));
        *statement_date = gnc_date_edit_get_date_end(GNC_DATE_EDIT(date_value));

        if (gnc_reverse_balance(account))
            *new_ending = gnc_numeric_neg (*new_ending);

        xaccAccountSetReconcileChildrenStatus(account, data.include_children);

        gnc_save_reconcile_interval(account, *statement_date);
    }
    // must remove the focus-out handler
    g_signal_handler_disconnect (G_OBJECT(entry), fo_handler_id);
//FIXME gtk4    gtk_window_destroy (GTK_WINDOW(dialog));
    g_object_unref(G_OBJECT(builder));

    return (result == GTK_RESPONSE_OK);
}


static void
gnc_reconcile_window_set_sensitivity(RecnWindow *recnData)
{
    gboolean sensitive = FALSE;
    GNCReconcileView *view;
    GAction *action;

    view = GNC_RECONCILE_VIEW(recnData->debit);
    if (gnc_reconcile_view_num_selected(view) == 1)
        sensitive = TRUE;

    view = GNC_RECONCILE_VIEW(recnData->credit);
    if (gnc_reconcile_view_num_selected(view) == 1)
        sensitive = TRUE;

    action = g_action_map_lookup_action (G_ACTION_MAP(recnData->simple_action_group),
                                         "TransEditAction");
    g_simple_action_set_enabled (G_SIMPLE_ACTION(action), sensitive);

    action = g_action_map_lookup_action (G_ACTION_MAP(recnData->simple_action_group),
                                         "TransDeleteAction");
    g_simple_action_set_enabled (G_SIMPLE_ACTION(action), sensitive);

    sensitive = FALSE;

    view = GNC_RECONCILE_VIEW(recnData->debit);
    if (gnc_reconcile_view_num_selected(view) > 0)
        sensitive = TRUE;

    view = GNC_RECONCILE_VIEW(recnData->credit);
    if (gnc_reconcile_view_num_selected(view) > 0)
        sensitive = TRUE;

    action = g_action_map_lookup_action (G_ACTION_MAP(recnData->simple_action_group),
                                         "TransRecAction");
    g_simple_action_set_enabled (G_SIMPLE_ACTION(action), sensitive);

    action = g_action_map_lookup_action (G_ACTION_MAP(recnData->simple_action_group),
                                         "TransUnRecAction");
    g_simple_action_set_enabled (G_SIMPLE_ACTION(action), sensitive);
}


static void
gnc_reconcile_window_toggled_cb(GNCReconcileView *view, Split *split,
                                gpointer data)
{
    auto recnData = static_cast<RecnWindow*>(data);
    gnc_reconcile_window_set_sensitivity(recnData);
    recnRecalculateBalance(recnData);
}


static void
gnc_reconcile_window_row_cb(GNCReconcileView *view, gpointer item,
                            gpointer data)
{
    auto recnData = static_cast<RecnWindow*>(data);
    gnc_reconcile_window_set_sensitivity(recnData);
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
do_popup_menu(RecnWindow *recnData, const GdkEvent *event)
{
//FIXME gtk4    GMenuModel *menu_model = (GMenuModel *)gtk_builder_get_object (recnData->builder,
//                                                                   "recwin-popup");
//FIXME gtk4    GtkWidget *menu = gtk_menu_new_from_model (menu_model);

//FIXME gtk4    if (!menu)
//        return;

//FIXME gtk4    gtk_menu_attach_to_widget (GTK_MENU(menu), GTK_WIDGET(recnData->window), NULL);
//FIXME gtk4    gtk_menu_popup_at_pointer (GTK_MENU(menu), event);
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
                                    RecnWindow *recnData)
{
    do_popup_menu(recnData, NULL);
    return TRUE;
}


/*  Callback function invoked when the user clicks in the content of
 *  any Gnucash window.  If this was a "right-click" then Gnucash will
 *  popup the contextual menu.
 */
//FIXME gtk4
#ifdef skip
static gboolean
gnc_reconcile_window_button_press_cb (GtkWidget *widget,
                                      const GdkEvent *event,
                                      RecnWindow *recnData)
{
    guint button;

    if (!gdk_event_get_button (event, &button))
        return FALSE;

    if (gdk_event_get_event_type (event) == GDK_BUTTON_PRESS && button == 3)
    {
        GNCQueryView *qview = GNC_QUERY_VIEW(widget);
        gdouble x_win, y_win;

        if (gdk_event_get_position ((GdkEvent*)event, &x_win, &y_win))
        {
            GtkTreePath *path;
            /* Get tree path for row that was clicked */
            gtk_tree_view_get_path_at_pos (GTK_TREE_VIEW(qview),
                                           (gint) x_win,
                                           (gint) y_win,
                                           &path, NULL, NULL, NULL);

            if (path)
            {
                GtkTreeSelection *selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(qview));

                gtk_tree_selection_select_path (selection, path);
                gtk_tree_path_free (path);
            }
        }
        do_popup_menu (recnData, event);
        return TRUE;
    }
    return FALSE;
}
#endif

static GNCSplitReg *
gnc_reconcile_window_open_register(RecnWindow *recnData)
{
    Account *account = recn_get_account (recnData);
    GNCSplitReg *gsr;
    gboolean include_children;

    if (!account)
        return(NULL);

    include_children = xaccAccountGetReconcileChildrenStatus (account);
    recnData->page = gnc_plugin_page_register_new (account, include_children);
    gnc_main_window_open_page (NULL, recnData->page);
    gsr = gnc_plugin_page_register_get_gsr (recnData->page);
    gnc_split_reg_raise (gsr);
    return gsr;
}


static void
gnc_reconcile_window_double_click_cb(GNCReconcileView *view, Split *split,
                                     gpointer data)
{
    auto recnData = static_cast<RecnWindow*>(data);
    GNCSplitReg *gsr;

    /* This should never be true, but be paranoid */
    if (split == NULL)
        return;

    gsr = gnc_reconcile_window_open_register(recnData);
    if (gsr == NULL)
        return;

    /* Test for visibility of split */
    if (gnc_split_reg_clear_filter_for_split (gsr, split))
        gnc_plugin_page_register_clear_current_filter (GNC_PLUGIN_PAGE(recnData->page));

    gnc_split_reg_jump_to_split( gsr, split );
}


static void
gnc_reconcile_window_focus_cb (GtkEventControllerFocus *controller,
                               gpointer user_data)
{
    auto recnData = static_cast<RecnWindow*>(user_data);
    GNCReconcileView *this_view, *other_view;
    GNCReconcileView *debit, *credit;

    GtkWidget *widget = gtk_event_controller_get_widget (GTK_EVENT_CONTROLLER(controller));

    this_view = GNC_RECONCILE_VIEW(widget);

    debit  = GNC_RECONCILE_VIEW(recnData->debit);
    credit = GNC_RECONCILE_VIEW(recnData->credit);

    other_view = GNC_RECONCILE_VIEW(this_view == debit ? credit : debit);

    /* clear the *other* list so we always have no more than one selection */
    gnc_reconcile_view_unselect_all(other_view);
}


static gboolean
gnc_reconcile_key_press_cb (GtkEventControllerKey *key, guint keyval,
                            guint keycode, GdkModifierType state,
                            gpointer user_data)
{
    auto recnData = static_cast<RecnWindow*>(user_data);
    GtkWidget *this_view, *other_view;
    GtkWidget *debit, *credit;

    switch (keyval)
    {
    case GDK_KEY_Tab:
    case GDK_KEY_ISO_Left_Tab:
        break;

    default:
        return FALSE;
    }

    GtkWidget *widget = gtk_event_controller_get_widget (GTK_EVENT_CONTROLLER(key));

    g_signal_stop_emission_by_name (widget, "key_press_event");

    this_view = widget;

    debit  = recnData->debit;
    credit = recnData->credit;

    other_view = (this_view == debit ? credit : debit);

    gtk_widget_grab_focus (other_view);

    return TRUE;
}


static void
gnc_reconcile_window_set_titles(RecnWindow *recnData)
{
    const gchar *title;

    title = gnc_account_get_debit_string(ACCT_TYPE_NONE);
    gtk_frame_set_label(GTK_FRAME(recnData->debit_frame), title);

    title = gnc_account_get_credit_string(ACCT_TYPE_NONE);
    gtk_frame_set_label(GTK_FRAME(recnData->credit_frame), title);
}


static GtkWidget *
gnc_reconcile_window_create_view_box(Account *account,
                                     GNCReconcileViewType type,
                                     RecnWindow *recnData,
                                     GtkWidget **list_save,
                                     GtkWidget **total_save)
{
    GtkWidget *frame, *scrolled_window, *view, *vbox, *label, *hbox;
    GtkWidget *vscroll;
    GtkRequisition nat_sb;

    frame = gtk_frame_new (NULL);

    if (type == RECLIST_DEBIT)
        recnData->debit_frame = frame;
    else
        recnData->credit_frame = frame;

    vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_set_homogeneous (GTK_BOX (vbox), FALSE);

    view = gnc_reconcile_view_new(account, type, recnData->statement_date);
    *list_save = view;

    g_signal_connect(view, "toggle_reconciled",
                     G_CALLBACK(gnc_reconcile_window_toggled_cb),
                     recnData);
    g_signal_connect(view, "line_selected",
                     G_CALLBACK(gnc_reconcile_window_row_cb),
                     recnData);
//FIXME gtk4    g_signal_connect(view, "button_press_event",
//                     G_CALLBACK(gnc_reconcile_window_button_press_cb),
//                     recnData);
    g_signal_connect(view, "double_click_split",
                     G_CALLBACK(gnc_reconcile_window_double_click_cb),
                     recnData);

    GtkEventController *event_controller1 = gtk_event_controller_focus_new ();
    gtk_widget_add_controller (GTK_WIDGET(view), event_controller1);
    g_signal_connect (G_OBJECT(event_controller1), "enter",
                      G_CALLBACK(gnc_reconcile_window_focus_cb),
                      recnData);

    GtkEventController *event_controller2 = gtk_event_controller_key_new ();
    gtk_widget_add_controller (GTK_WIDGET(view), event_controller2);
    g_signal_connect (event_controller2, 
                      "key-pressed",
                      G_CALLBACK(gnc_reconcile_key_press_cb), recnData);

    scrolled_window = gtk_scrolled_window_new ();
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW (scrolled_window),
                                   GTK_POLICY_AUTOMATIC,
                                   GTK_POLICY_AUTOMATIC);
//FIXME gtk4    gnc_box_set_all_margins (GTK_BOX(scrollwin), 5);

    gtk_frame_set_child (GTK_FRAME(frame), GTK_WIDGET(scrolled_window));
    gtk_box_append (GTK_BOX(scrolled_window), GTK_WIDGET(view));
    gtk_scrolled_window_set_child (GTK_SCROLLED_WINDOW(scrolled_window),
                                   GTK_WIDGET(view));
    gtk_box_append (GTK_BOX(vbox), GTK_WIDGET(frame));

    // get the vertical scroll bar width
    vscroll = gtk_scrolled_window_get_vscrollbar (GTK_SCROLLED_WINDOW(scrolled_window));
    gtk_widget_get_preferred_size (vscroll, NULL, &nat_sb);

    // add xpadding to recn column so scrollbar does not cover
    gnc_reconcile_view_add_padding (GNC_RECONCILE_VIEW(view), REC_RECN, nat_sb.width);

    hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 5);
    gtk_box_set_homogeneous (GTK_BOX (hbox), FALSE);
    gtk_box_append (GTK_BOX(vbox), GTK_WIDGET(hbox));

    label = gtk_label_new(_("Total"));
    gnc_label_set_alignment(label, 1.0, 0.5);
    gtk_box_append (GTK_BOX(hbox), GTK_WIDGET(label));

    label = gtk_label_new("");
    gtk_box_append (GTK_BOX(hbox), GTK_WIDGET(label));
    *total_save = label;
    gtk_widget_set_margin_end (GTK_WIDGET(label), 10 + nat_sb.width);

    return vbox;
}


static Split *
gnc_reconcile_window_get_current_split(RecnWindow *recnData)
{
    GNCReconcileView *view;
    Split *split;

    view = GNC_RECONCILE_VIEW(recnData->debit);
    split = gnc_reconcile_view_get_current_split(view);
    if (split != NULL)
        return split;

    view = GNC_RECONCILE_VIEW(recnData->credit);
    split = gnc_reconcile_view_get_current_split(view);

    return split;
}


static void
gnc_ui_reconcile_window_help_cb (GSimpleAction *simple,
                                 GVariant      *parameter,
                                 gpointer       user_data)
{
    auto recnData = static_cast<RecnWindow*>(user_data);
    gnc_gnome_help (GTK_WINDOW(recnData->window), DF_MANUAL, DL_RECNWIN);
}


static void
gnc_ui_reconcile_window_change_cb (GSimpleAction *simple,
                                   GVariant      *parameter,
                                   gpointer       user_data)
{
    auto recnData = static_cast<RecnWindow*>(user_data);
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
gnc_ui_reconcile_window_balance_cb (GSimpleAction *simple,
                                    GVariant      *parameter,
                                    gpointer       user_data)
{
    auto recnData = static_cast<RecnWindow*>(user_data);
    GNCSplitReg *gsr;
    Account *account;
    gnc_numeric balancing_amount;
    time64 statement_date;


    gsr = gnc_reconcile_window_open_register(recnData);
    if (gsr == NULL)
        return;

    account = recn_get_account(recnData);
    if (account == NULL)
        return;

    balancing_amount = recnRecalculateBalance(recnData);
    if (gnc_numeric_zero_p(balancing_amount))
        return;

    statement_date = recnData->statement_date;
    if (statement_date == 0)
        statement_date = gnc_time (NULL); // default to 'now'

    gnc_split_reg_balancing_entry(gsr, account, statement_date, balancing_amount);
}


static void
gnc_ui_reconcile_window_rec_cb (GSimpleAction *simple,
                                GVariant      *parameter,
                                gpointer       user_data)
{
    auto recnData = static_cast<RecnWindow*>(user_data);
    GNCReconcileView *debit, *credit;

    debit  = GNC_RECONCILE_VIEW(recnData->debit);
    credit = GNC_RECONCILE_VIEW(recnData->credit);

    gnc_reconcile_view_set_list (debit, TRUE);
    gnc_reconcile_view_set_list (credit, TRUE);
}


static void
gnc_ui_reconcile_window_unrec_cb (GSimpleAction *simple,
                                  GVariant      *parameter,
                                  gpointer       user_data)
{
    auto recnData = static_cast<RecnWindow*>(user_data);
    GNCReconcileView *debit, *credit;

    debit  = GNC_RECONCILE_VIEW(recnData->debit);
    credit = GNC_RECONCILE_VIEW(recnData->credit);

    gnc_reconcile_view_set_list (debit, FALSE);
    gnc_reconcile_view_set_list (credit, FALSE);
}


/** Get the debit or credit view that has at least 1 split selected.
 *   gnc_reconcile_window_focus_cb() ensures only 1 view
 *   has a selection.
 * @param window The reconcile window.
 */
static GNCReconcileView *
gnc_reconcile_window_get_selection_view (RecnWindow *recnData)
{
    if (gnc_reconcile_view_num_selected (GNC_RECONCILE_VIEW (recnData->debit)) > 0)
        return GNC_RECONCILE_VIEW (recnData->debit);

    if (gnc_reconcile_view_num_selected (GNC_RECONCILE_VIEW (recnData->credit)) > 0)
        return GNC_RECONCILE_VIEW (recnData->credit);

    return NULL;
}


/** Select the next split in the debit or credit view so that after the Delete
 *   button is actioned, the working position in the list is still in view.
 *  Unless this is done, the list will be scrolled to the top.
 *  The new split selected must have a different parent transaction as all splits
 *   for the transaction will be deleted.
 */
static void
gnc_reconcile_window_delete_set_next_selection (RecnWindow *recnData, Split *split)
{
    GNCReconcileView *view = gnc_reconcile_window_get_selection_view (recnData);
    GtkTreeModel *model = gtk_tree_view_get_model (GTK_TREE_VIEW (view));
    Split *this_split = NULL;
    GtkTreeIter iter;
    GtkTreeSelection *selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (view));
    GList *path_list, *node;
    GtkTreePath *save_del_path;
    Transaction* trans = xaccSplitGetParent (split); // parent transaction of the split to delete

    if (!view)
        return; // no selected split

    path_list = gtk_tree_selection_get_selected_rows (selection, &model);
    // get path of the first split selected - there should be only 1 selected
    node = g_list_first (path_list);
    if (!node)
        return;
    auto path = static_cast<GtkTreePath*>(node->data);
    save_del_path = gtk_tree_path_copy (path);

    gtk_tree_path_next (path);
    if (gtk_tree_model_get_iter (model, &iter, path))
    {
        do
        {
            gtk_tree_model_get (model, &iter, REC_POINTER, &this_split, -1);
        }
        while (xaccSplitGetParent (this_split) == trans && gtk_tree_model_iter_next (model, &iter));
    }

    if ((!this_split) || xaccSplitGetParent (this_split) == trans)
    {
        // There aren't any splits for a different transaction after the split to be deleted,
        //  so find the previous split having a different parent transaction
        path = save_del_path; // split to be deleted
        if (gtk_tree_path_prev (path) && gtk_tree_model_get_iter (model, &iter, path))
        {
            do
            {
                gtk_tree_model_get (model, &iter, REC_POINTER, &this_split, -1);
            }
            while (xaccSplitGetParent (this_split) == trans && gtk_tree_model_iter_previous (model, &iter));
        }
    }

    gtk_tree_path_free (save_del_path);
    g_list_free_full (path_list, (GDestroyNotify) gtk_tree_path_free);
    if ((!this_split) || xaccSplitGetParent (this_split) == trans)
        return;

    gtk_tree_selection_select_iter (selection, &iter);
}


static void
gnc_ui_reconcile_window_delete_cb (GSimpleAction *simple,
                                   GVariant      *parameter,
                                   gpointer       user_data)
{
    auto recnData = static_cast<RecnWindow*>(user_data);
    Transaction *trans;
    Split *split;

    split = gnc_reconcile_window_get_current_split(recnData);
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

    /* select the split that should be visible after the deletion */
    gnc_reconcile_window_delete_set_next_selection(recnData, split);

    gnc_suspend_gui_refresh ();

    trans = xaccSplitGetParent(split);
    xaccTransDestroy(trans);

    gnc_resume_gui_refresh ();
}


static void
gnc_ui_reconcile_window_edit_cb (GSimpleAction *simple,
                                 GVariant      *parameter,
                                 gpointer       user_data)
{
    auto recnData = static_cast<RecnWindow*>(user_data);
    GNCSplitReg *gsr;
    Split *split;

    split = gnc_reconcile_window_get_current_split (recnData);
    /* This should never be true, but be paranoid */
    if (split == NULL)
        return;

    gsr = gnc_reconcile_window_open_register(recnData);
    if (gsr == NULL)
        return;

    /* Test for visibility of split */
    if (gnc_split_reg_clear_filter_for_split (gsr, split))
        gnc_plugin_page_register_clear_current_filter (GNC_PLUGIN_PAGE(recnData->page));

    gnc_split_reg_jump_to_split_amount( gsr, split );
}


static char *
gnc_recn_make_window_name(Account *account)
{
    char *fullname;
    char *title;

    fullname = gnc_account_get_full_name(account);
    title = g_strconcat(fullname, " - ", _("Reconcile"), NULL);

    g_free(fullname);

    return title;
}


static void
gnc_recn_set_window_name(RecnWindow *recnData)
{
    char *title;

    title = gnc_recn_make_window_name (recn_get_account (recnData));

    gtk_window_set_title (GTK_WINDOW (recnData->window), title);

    g_free (title);
}


static void
gnc_recn_edit_account_cb (GSimpleAction *simple,
                          GVariant      *parameter,
                          gpointer       user_data)
{
    auto recnData = static_cast<RecnWindow*>(user_data);
    Account *account = recn_get_account (recnData);

    if (account == NULL)
        return;

    gnc_ui_edit_account_window (GTK_WINDOW (recnData->window), account);
}


static void
gnc_recn_xfer_cb (GSimpleAction *simple,
                  GVariant      *parameter,
                  gpointer       user_data)
{
    auto recnData = static_cast<RecnWindow*>(user_data);
    Account *account = recn_get_account (recnData);

    if (account == NULL)
        return;

    gnc_xfer_dialog (recnData->window, account);
}


static void
gnc_recn_scrub_cb (GSimpleAction *simple,
                   GVariant      *parameter,
                   gpointer       user_data)
{
    auto recnData = static_cast<RecnWindow*>(user_data);
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
gnc_recn_open_cb (GSimpleAction *simple,
                  GVariant      *parameter,
                  gpointer       user_data)
{
    auto recnData = static_cast<RecnWindow*>(user_data);

    gnc_reconcile_window_open_register(recnData);
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

    always_today = gnc_prefs_get_bool(GNC_PREFS_GROUP_RECONCILE, GNC_PREF_ALWAYS_REC_TO_TODAY);

    if (!always_today &&
            xaccAccountGetReconcileLastDate (account, statement_date))
    {
        int months = 1, days = 0;

        gnc_gdate_set_time64(&date, *statement_date);

        xaccAccountGetReconcileLastInterval (account, &months, &days);

        if (months)
        {
            gboolean was_last_day_of_month = g_date_is_last_of_month(&date);

            g_date_add_months(&date, months);

            /* Track last day of the month, i.e. 1/31 -> 2/28 -> 3/31 */
            if (was_last_day_of_month)
            {
                g_date_set_day (&date, g_date_get_days_in_month(g_date_get_month(&date),
                                g_date_get_year( &date)));
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

    if (xaccAccountGetReconcilePostponeBalance(account, new_ending))
    {
        if (gnc_reverse_balance(account))
            *new_ending = gnc_numeric_neg(*new_ending);
    }
    else
    {
        /* if the account wasn't previously postponed, try to predict
         * the statement balance based on the statement date.
         */
        *new_ending =
            gnc_ui_account_get_balance_as_of_date
            (account, *statement_date,
             xaccAccountGetReconcileChildrenStatus(account));
    }
}


static gboolean
find_by_account (gpointer find_data, gpointer user_data)
{
    auto account = GNC_ACCOUNT(find_data);
    auto recnData = static_cast<RecnWindow*>(user_data);

    if (!recnData)
        return FALSE;

    return guid_equal (&recnData->account, xaccAccountGetGUID (account));
}


static void
recn_set_watches_one_account (gpointer data, gpointer user_data)
{
    Account *account = (Account *)data;
    RecnWindow *recnData = (RecnWindow *)user_data;

    /* add a watch on the account */
    gnc_gui_component_watch_entity (recnData->component_id,
                                    xaccAccountGetGUID (account),
                                    QOF_EVENT_MODIFY | QOF_EVENT_DESTROY);

    /* add a watch on each unreconciled or cleared split for the account */
    for (auto split : xaccAccountGetSplits (account))
    {
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
recn_set_watches (RecnWindow *recnData)
{
    gboolean include_children;
    Account *account;
    GList *accounts = NULL;

    gnc_gui_component_clear_watches (recnData->component_id);

    account = recn_get_account (recnData);

    include_children = xaccAccountGetReconcileChildrenStatus(account);
    if (include_children)
        accounts = gnc_account_get_descendants(account);

    /* match the account */
    accounts = g_list_prepend (accounts, account);

    g_list_foreach(accounts, recn_set_watches_one_account, recnData);

    g_list_free (accounts);
}


static void
refresh_handler (GHashTable *changes, gpointer user_data)
{
    auto recnData = static_cast<RecnWindow*>(user_data);
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

    gnc_reconcile_window_set_titles(recnData);
    recn_set_watches (recnData);

    recnRefresh (recnData);
}


static void
close_handler (gpointer user_data)
{
    auto recnData = static_cast<RecnWindow*>(user_data);

    gnc_save_window_size(GNC_PREFS_GROUP_RECONCILE, GTK_WINDOW(recnData->window));
//FIXME gtk4    gtk_window_destroy (GTK_WINDOW(recnData->window));
}


/********************************************************************\
 * recnWindow                                                       *
 *   opens up the window to reconcile an account                    *
 *                                                                  *
 * Args:   parent  - the parent of this window                      *
 *         account - the account to reconcile                       *
 * Return: recnData - the instance of this RecnWindow               *
\********************************************************************/
RecnWindow *
recnWindow (GtkWidget *parent, Account *account)
{
    gnc_numeric new_ending;
    gboolean enable_subaccounts;
    time64 statement_date;

    if (account == NULL)
        return NULL;

    /* The last time reconciliation was attempted during the current execution
     * of gnucash, the date was stored. Use that date if possible. This helps
     * with balancing multiple accounts for which statements are issued at the
     * same time, like multiple bank accounts on a single statement. Otherwise
     * use the end of today to ensure we include any transactions posted
     * today.
     */
    if (!gnc_reconcile_last_statement_date)
        statement_date = gnc_time64_get_day_end(gnc_time (NULL));
    else
        statement_date = gnc_reconcile_last_statement_date;

    gnc_get_reconcile_info (account, &new_ending, &statement_date);

    enable_subaccounts = !has_account_different_commodities(account);
    /* Popup a little window to prompt the user to enter the
     * ending balance for his/her bank statement */
    if (!startRecnWindow (parent, account, &new_ending, &statement_date,
            enable_subaccounts))
        return NULL;

    return recnWindowWithBalance (parent, account, new_ending, statement_date);
}


static GActionEntry recWindow_actions_entries [] =
{
    { "RecnChangeInfoAction", gnc_ui_reconcile_window_change_cb, NULL, NULL, NULL },
    { "RecnFinishAction", recnFinishCB, NULL, NULL, NULL },
    { "RecnPostponeAction", recnPostponeCB, NULL, NULL, NULL },
    { "RecnCancelAction", recnCancelCB, NULL, NULL, NULL },

    { "AccountOpenAccountAction", gnc_recn_open_cb, NULL, NULL, NULL },
    { "AccountEditAccountAction", gnc_recn_edit_account_cb, NULL, NULL, NULL },
    { "AccountTransferAction", gnc_recn_xfer_cb, NULL, NULL, NULL },
    { "AccountCheckRepairAction", gnc_recn_scrub_cb, NULL, NULL, NULL },

    { "TransBalanceAction", gnc_ui_reconcile_window_balance_cb, NULL, NULL, NULL },
    { "TransEditAction", gnc_ui_reconcile_window_edit_cb, NULL, NULL, NULL },
    { "TransDeleteAction", gnc_ui_reconcile_window_delete_cb, NULL, NULL, NULL },
    { "TransRecAction", gnc_ui_reconcile_window_rec_cb, NULL, NULL, NULL },
    { "TransUnRecAction", gnc_ui_reconcile_window_unrec_cb, NULL, NULL, NULL },

    { "HelpHelpAction", gnc_ui_reconcile_window_help_cb, NULL, NULL, NULL },
};
/** The number of actions provided by the reconcile window. */
static guint recnWindow_n_actions_entries = G_N_ELEMENTS(recWindow_actions_entries);

#ifdef MAC_INTEGRATION
/* Enable GtkMenuItem accelerators */
static gboolean
can_activate_cb(GtkWidget *widget, guint signal_id, gpointer data)
{
    //return gtk_widget_is_sensitive (widget);
    return TRUE;
}
#endif

/********************************************************************\
 * recnWindowWithBalance
 *
 *   Opens up the window to reconcile an account, but with ending
 *   balance and statement date already given.
 *
 * Args:   parent         - The parent widget of the new window
 *         account        - The account to reconcile
 *         new_ending     - The amount for ending balance
 *         statement_date - The date of the statement
 * Return: recnData - the instance of this RecnWindow
\********************************************************************/
RecnWindow *
recnWindowWithBalance (GtkWidget *parent, Account *account, gnc_numeric new_ending,
                       time64 statement_date)
{
    RecnWindow *recnData;
    GtkWidget *statusbar;
    GtkWidget *vbox;
    GtkWidget *dock;

    if (account == NULL)
        return NULL;

    recnData = static_cast<RecnWindow*>(gnc_find_first_gui_component (WINDOW_RECONCILE_CM_CLASS,
                                                                      find_by_account, account));
    if (recnData)
        return recnData;

    recnData = g_new0 (RecnWindow, 1);

    recnData->account = *xaccAccountGetGUID (account);


    recnData->component_id =
        gnc_register_gui_component (WINDOW_RECONCILE_CM_CLASS,
                                    refresh_handler, close_handler,
                                    recnData);
    gnc_gui_component_set_session (recnData->component_id, gnc_get_current_session());

    recn_set_watches (recnData);

    gnc_reconcile_last_statement_date = statement_date;

    recnData->new_ending = new_ending;
    recnData->statement_date = statement_date;
    recnData->window = gtk_window_new();
    recnData->delete_refresh = FALSE;

    gnc_recn_set_window_name(recnData);

    vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
    gtk_box_set_homogeneous (GTK_BOX (vbox), FALSE);
    gtk_window_set_child (GTK_WINDOW(recnData->window), GTK_WIDGET(vbox));

    // Set the name for this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(recnData->window), "gnc-id-reconcile");

    dock = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
    gtk_box_set_homogeneous (GTK_BOX (dock), FALSE);
    gtk_widget_set_visible (GTK_WIDGET(dock), TRUE);
    gtk_box_append (GTK_BOX(vbox), GTK_WIDGET(dock));

    {
//FIXME gtk4        GtkToolbar *tool_bar;
//        GMenuModel *menu_model;
//        GtkWidget *menu_bar;
//        GtkAccelGroup *accel_group = gtk_accel_group_new ();
        GtkEventController *shortcut_controller = gtk_shortcut_controller_new ();

        const gchar *ui = GNUCASH_RESOURCE_PREFIX "/gnc-reconcile-window.ui";
        GError *error = NULL;

        recnData->builder = gtk_builder_new ();

        gtk_builder_add_from_resource (recnData->builder, ui, &error);

        gtk_builder_set_translation_domain (recnData->builder, PROJECT_NAME);

        if (error)
        {
            g_critical ("Failed to load ui resource %s, Error %s", ui, error->message);
            g_error_free (error);
            gnc_unregister_gui_component_by_data (WINDOW_RECONCILE_CM_CLASS, recnData);
            g_free (recnData);
            return NULL;
        }

//FIXME gtk4        menu_model = (GMenuModel *)gtk_builder_get_object (recnData->builder, "recwin-menu");
//        menu_bar = gtk_menu_bar_new_from_model (menu_model);
//        gtk_box_prepend (GTK_BOX(vbox), GTK_WIDGET(menu_bar));
#ifdef MAC_INTEGRATION
        auto theApp = static_cast<GtkosxApplication*>(g_object_new (GTKOSX_TYPE_APPLICATION, NULL));
        gtk_widget_set_visible (GTK_WIDGET(menu_bar), FALSE);
//FIXME gtk4        gtk_widget_set_no_show_all (menu_bar, TRUE);
        if (GTK_IS_MENU_ITEM (menu_bar))
            menu_bar = gtk_menu_item_get_submenu (GTK_MENU_ITEM (menu_bar));

        gtkosx_application_set_menu_bar (theApp, GTK_MENU_SHELL (menu_bar));
#endif
//FIXME gtk4        tool_bar = (GtkToolbar *)gtk_builder_get_object (recnData->builder, "recwin-toolbar");

//        gtk_toolbar_set_style (GTK_TOOLBAR(tool_bar), GTK_TOOLBAR_BOTH);
//        gtk_toolbar_set_icon_size (GTK_TOOLBAR(tool_bar),
//                                   GTK_ICON_SIZE_SMALL_TOOLBAR);

//        gtk_box_prepend (GTK_BOX(vbox), GTK_WIDGET(tool_bar));

        gtk_widget_add_controller (GTK_WIDGET(recnData->window), GTK_EVENT_CONTROLLER(shortcut_controller));

        // need to add the accelerator keys
//        gnc_add_accelerator_keys_for_menu (menu_bar, menu_model, shortcut_controller);

#ifdef MAC_INTEGRATION
        gtkosx_application_sync_menubar (theApp);
        g_signal_connect (menu_bar, "can-activate-accel",
                          G_CALLBACK(can_activate_cb), NULL);
        g_object_unref (theApp);
        theApp = NULL;
#endif

        recnData->simple_action_group = g_simple_action_group_new ();

        g_action_map_add_action_entries (G_ACTION_MAP(recnData->simple_action_group),
                                         recWindow_actions_entries,
                                         recnWindow_n_actions_entries,
                                         recnData);

        gtk_widget_insert_action_group (GTK_WIDGET(recnData->window), "recwin",
                                        G_ACTION_GROUP(recnData->simple_action_group));
    }

    g_signal_connect(recnData->window, "popup-menu",
                     G_CALLBACK(gnc_reconcile_window_popup_menu_cb), recnData);

    statusbar = gtk_statusbar_new();
    gtk_box_prepend (GTK_BOX(vbox), GTK_WIDGET(statusbar));

    g_signal_connect (recnData->window, "destroy",
                      G_CALLBACK(recn_destroy_cb), recnData);
    g_signal_connect (recnData->window, "delete_event",
                      G_CALLBACK(recn_delete_cb), recnData);

    GtkEventController *event_controller = gtk_event_controller_key_new ();
    gtk_widget_add_controller (GTK_WIDGET(recnData->window), event_controller);
    g_signal_connect (event_controller, 
                      "key-pressed",
                      G_CALLBACK(recn_key_press_cb), recnData);

    /* if account has a reconciled split where reconciled_date is
       later than statement_date, emit a warning into statusbar */
    {
        GtkStatusbar *bar = GTK_STATUSBAR (statusbar);
        guint context = gtk_statusbar_get_context_id (bar, "future_dates");
//FIXME gtk4        GtkWidget *box = gtk_statusbar_get_message_area (bar);
//        GtkWidget *image = gtk_image_new_from_icon_name ("dialog-warning");
//        gtk_image_set_icon_size (GTK_IMAGE(image), GTK_ICON_SIZE_NORMAL);

        auto find_split = [statement_date](const Split *split)
        { return (xaccSplitGetReconcile (split) == YREC &&
                  xaccSplitGetDateReconciled (split) > statement_date); };

        if (auto split = gnc_account_find_split (account, find_split, true))
        {
            auto datestr = qof_print_date (xaccTransGetDate (xaccSplitGetParent (split)));
            auto recnstr = qof_print_date (xaccSplitGetDateReconciled (split));
            PWARN ("split posting_date=%s, recn_date=%s", datestr, recnstr);

            gtk_statusbar_push (bar, context, _("WARNING! Account contains \
splits whose reconcile date is after statement date. Reconciliation may be \
difficult."));

            gtk_widget_set_tooltip_text (GTK_WIDGET (bar), _("This account \
has splits whose Reconciled Date is after this reconciliation statement date. \
These splits may make reconciliation difficult. If this is the case, you may \
use Find Transactions to find them, unreconcile, and re-reconcile."));

//FIXME gtk4            gtk_box_append (GTK_BOX(box), GTK_WIDGET(image));
//            gtk_box_reorder_child_after (GTK_BOX(box), GTK_WIDGET(image), nullptr);

            g_free (datestr);
            g_free (recnstr);
        }
    }

    /* The main area */
    {
        GtkWidget *frame = gtk_frame_new (NULL);
        GtkWidget *main_area = gtk_box_new (GTK_ORIENTATION_VERTICAL, 10);
        GtkWidget *debcred_area = gtk_grid_new ();
        GtkWidget *debits_box;
        GtkWidget *credits_box;

        gtk_box_set_homogeneous (GTK_BOX (main_area), FALSE);
        gtk_box_append (GTK_BOX(vbox), GTK_WIDGET(frame));
        gtk_box_set_spacing (GTK_BOX(vbox), 10);

        /* Force a reasonable starting size */
        gtk_window_set_default_size(GTK_WINDOW(recnData->window), 800, 600);
        gnc_restore_window_size (GNC_PREFS_GROUP_RECONCILE,
                                 GTK_WINDOW(recnData->window), GTK_WINDOW(parent));

        gtk_frame_set_child (GTK_FRAME(frame), GTK_WIDGET(main_area));
        gnc_box_set_all_margins (GTK_BOX(main_area), 10);

        debits_box = gnc_reconcile_window_create_view_box
                     (account, RECLIST_DEBIT, recnData,
                      &recnData->debit, &recnData->total_debit);

        // Add a style context for this widget so it can be easily manipulated with css
        gnc_widget_style_context_add_class (GTK_WIDGET(debits_box), "gnc-class-debits");

        credits_box = gnc_reconcile_window_create_view_box
                      (account, RECLIST_CREDIT, recnData,
                       &recnData->credit, &recnData->total_credit);

        // Add a style context for this widget so it can be easily manipulated with css
        gnc_widget_style_context_add_class (GTK_WIDGET(credits_box), "gnc-class-credits");

        GNC_RECONCILE_VIEW(recnData->debit)->sibling = GNC_RECONCILE_VIEW(recnData->credit);
        GNC_RECONCILE_VIEW(recnData->credit)->sibling = GNC_RECONCILE_VIEW(recnData->debit);

        gtk_box_append (GTK_BOX(main_area), GTK_WIDGET(debcred_area));

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
            gtk_box_append (GTK_BOX(main_area), GTK_WIDGET(hbox));

            /* frame to hold totals */
            frame = gtk_frame_new (NULL);
            gtk_box_prepend (GTK_BOX(hbox), GTK_WIDGET(frame));

            // Set the name for this widget so it can be easily manipulated with css
            gtk_widget_set_name (GTK_WIDGET(frame), "gnc-id-reconcile-totals");

            /* hbox to hold title/value vboxes */
            totals_hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 3);
            gtk_box_set_homogeneous (GTK_BOX (totals_hbox), FALSE);
            gtk_frame_set_child (GTK_FRAME(frame), GTK_WIDGET(totals_hbox));
            gnc_box_set_all_margins (GTK_BOX(totals_hbox), 5);
            /* vbox to hold titles */
            title_vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 3);
            gtk_box_set_homogeneous (GTK_BOX (title_vbox), FALSE);
            gtk_box_append (GTK_BOX(totals_hbox), GTK_WIDGET(title_vbox));

            /* vbox to hold values */
            value_vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 3);
            gtk_box_set_homogeneous (GTK_BOX (value_vbox), FALSE);
            gtk_box_append (GTK_BOX(totals_hbox), GTK_WIDGET(value_vbox));

            /* statement date title/value */
            title = gtk_label_new(_("Statement Date"));
            gnc_label_set_alignment(title, 1.0, 0.5);
            gtk_box_append (GTK_BOX(title_vbox), GTK_WIDGET(title));

            value = gtk_label_new("");
            recnData->recn_date = value;
            gnc_label_set_alignment(value, 1.0, 0.5);
            gtk_box_append (GTK_BOX(value_vbox), GTK_WIDGET(value));

            /* starting balance title/value */
            title = gtk_label_new(_("Starting Balance"));
            gnc_label_set_alignment(title, 1.0, 0.5);
            gtk_box_append (GTK_BOX(title_vbox), GTK_WIDGET(title));
            gtk_box_set_spacing (GTK_BOX(title_vbox), 3);

            value = gtk_label_new("");
            recnData->starting = value;
            gnc_label_set_alignment(value, 1.0, 0.5);
            gtk_box_append (GTK_BOX(value_vbox), GTK_WIDGET(value));
            gtk_box_set_spacing (GTK_BOX(value_vbox), 3);

            /* ending balance title/value */
            title = gtk_label_new(_("Ending Balance"));
            gnc_label_set_alignment(title, 1.0, 0.5);
            gtk_box_append (GTK_BOX(title_vbox), GTK_WIDGET(title));

            value = gtk_label_new("");
            recnData->ending = value;
            gnc_label_set_alignment(value, 1.0, 0.5);
            gtk_box_append (GTK_BOX(value_vbox), GTK_WIDGET(value));

            /* reconciled balance title/value */
            title = gtk_label_new(_("Reconciled Balance"));
            gnc_label_set_alignment(title, 1.0, 0.5);
            gtk_box_append (GTK_BOX(title_vbox), GTK_WIDGET(title));

            value = gtk_label_new("");
            recnData->reconciled = value;
            gnc_label_set_alignment(value, 1.0, 0.5);
            gtk_box_append (GTK_BOX(value_vbox), GTK_WIDGET(value));

            /* difference title/value */
            title = gtk_label_new(_("Difference"));
            gnc_label_set_alignment(title, 1.0, 0.5);
            gtk_box_append (GTK_BOX(title_vbox), GTK_WIDGET(title));

            value = gtk_label_new("");
            recnData->difference = value;
            gnc_label_set_alignment(value, 1.0, 0.5);
            gtk_box_append (GTK_BOX(value_vbox), GTK_WIDGET(value));
        }

        /* Set up the data */
        recnRefresh (recnData);
    }

    /* Allow resize */
    gtk_window_set_resizable(GTK_WINDOW(recnData->window), TRUE);
//FIXME gtk4    gtk_widget_show_all(recnData->window);

    gnc_reconcile_window_set_titles(recnData);

    recnRecalculateBalance(recnData);

    gnc_window_adjust_for_screen(GTK_WINDOW(recnData->window));

    /* Set the sort orders of the debit and credit tree views */
    gnc_query_sort_order(GNC_QUERY_VIEW(recnData->debit), REC_DATE, GTK_SORT_ASCENDING);
    gnc_query_sort_order(GNC_QUERY_VIEW(recnData->credit), REC_DATE, GTK_SORT_ASCENDING);

    gtk_widget_grab_focus (recnData->debit);

    {   // align the Totals value with that of the amount column
        gint recn_widthc = gnc_reconcile_view_get_column_width (GNC_RECONCILE_VIEW(recnData->credit), REC_RECN);
        gint recn_widthd = gnc_reconcile_view_get_column_width (GNC_RECONCILE_VIEW(recnData->debit), REC_RECN);

        gtk_widget_set_margin_end (GTK_WIDGET(recnData->total_credit), 10 + recn_widthc);
        gtk_widget_set_margin_end (GTK_WIDGET(recnData->total_debit), 10 + recn_widthd);
    }
    return recnData;
}


/********************************************************************\
 * gnc_ui_reconcile_window_raise                                     *
 *   shows and raises an account editing window                     *
 *                                                                  *
 * Args:   editAccData - the edit window structure                  *
\********************************************************************/
void
gnc_ui_reconcile_window_raise(RecnWindow * recnData)
{
    if (recnData == NULL)
        return;

    if (recnData->window == NULL)
        return;

    gtk_window_present(GTK_WINDOW(recnData->window));
}

GtkWindow *
gnc_ui_reconcile_window_get_window (RecnWindow * recnData)
{
    if (recnData == NULL || recnData->window == NULL)
        return NULL;
    return GTK_WINDOW(recnData->window);
}



/********************************************************************\
 * recn_destroy_cb                                                  *
 *   frees memory allocated for an recnWindow, and other cleanup    *
 *   stuff                                                          *
 *                                                                  *
 * Args:   w    - the widget that called us                         *
 *         data - the data struct for this window                   *
 * Return: none                                                     *
\********************************************************************/
static void
recn_destroy_cb (GtkWidget *w, gpointer data)
{
    auto recnData = static_cast<RecnWindow*>(data);
    gchar **actions = g_action_group_list_actions (G_ACTION_GROUP(recnData->simple_action_group));
    gint num_actions = g_strv_length (actions);

    gnc_unregister_gui_component_by_data (WINDOW_RECONCILE_CM_CLASS, recnData);

    if (recnData->delete_refresh)
        gnc_resume_gui_refresh ();

    //Disable the actions, the handlers try to access recnData
    for (gint i = 0; i < num_actions; i++)
    {
        GAction *action = g_action_map_lookup_action (G_ACTION_MAP(recnData->simple_action_group), actions[i]);
        g_simple_action_set_enabled (G_SIMPLE_ACTION(action), FALSE);
    }
    g_strfreev (actions);
    g_free (recnData);
}


static void
recn_cancel(RecnWindow *recnData)
{
    gboolean changed = FALSE;

    if (gnc_reconcile_view_changed(GNC_RECONCILE_VIEW(recnData->credit)))
        changed = TRUE;
    if (gnc_reconcile_view_changed(GNC_RECONCILE_VIEW(recnData->debit)))
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
recn_delete_cb(GtkWidget *widget, const GdkEvent *event, gpointer data)
{
    auto recnData = static_cast<RecnWindow*>(data);

    recn_cancel(recnData);
    return TRUE;
}


static gboolean
recn_key_press_cb (GtkEventControllerKey *key, guint keyval,
                   guint keycode, GdkModifierType state,
                   gpointer user_data)
{
    auto recnData = static_cast<RecnWindow*>(user_data);

    if (keyval == GDK_KEY_Escape)
    {

        recn_cancel(recnData);
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
find_payment_account(Account *account)
{
    if (account == NULL)
        return NULL;

    GList *list = xaccAccountGetSplitList (account);
    Account *rv = nullptr;

    /* Search backwards to find the latest payment */
    for (GList *node = g_list_last (list); !rv && node; node = node->prev)
    {
        Transaction *trans;
        GList *n;

        auto split = GNC_SPLIT(node->data);
        if (split == NULL)
            continue;

        /* ignore 'purchases' */
        if (!gnc_numeric_positive_p (xaccSplitGetAmount(split)))
            continue;

        trans = xaccSplitGetParent(split);
        if (trans == NULL)
            continue;

        for (n = xaccTransGetSplitList (trans); n; n = n->next)
        {
            GNCAccountType type;
            Account *a;

            auto s = GNC_SPLIT(n->data);
            if ((s == NULL) || (s == split))
                continue;

            a = xaccSplitGetAccount(s);
            if ((a == NULL) || (a == account))
                continue;

            type = xaccAccountGetType(a);
            if ((type == ACCT_TYPE_BANK) || (type == ACCT_TYPE_CASH) ||
                    (type == ACCT_TYPE_ASSET))
                rv = a;
        }
    }

    g_list_free (list);
    return rv;
}

typedef void (*AccountProc) (Account *a);
static void traverse_fn (Account *acct, AccountProc fn)
{
    fn (acct);
}

static void
acct_traverse_descendants (Account *acct, AccountProc fn)
{
    fn (acct);
    if (xaccAccountGetReconcileChildrenStatus (acct))
        gnc_account_foreach_descendant (acct, (AccountCb)traverse_fn, (gpointer)fn);
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
recnFinishCB (GSimpleAction *simple,
              GVariant      *parameter,
              gpointer       user_data)
{
    auto recnData = static_cast<RecnWindow*>(user_data);
    gboolean auto_payment;
    Account *account;
    time64 date;

    if (!gnc_numeric_zero_p (recnRecalculateBalance(recnData)))
    {
        const char *message = _("The account is not balanced. "
                                "Are you sure you want to finish?");
        if (!gnc_verify_dialog (GTK_WINDOW (recnData->window), FALSE, "%s", message))
            return;
    }

    date = recnData->statement_date;

    gnc_suspend_gui_refresh ();

    recnData->delete_refresh = TRUE;
    account = recn_get_account (recnData);

    acct_traverse_descendants (account, xaccAccountBeginEdit);
    gnc_reconcile_view_commit(GNC_RECONCILE_VIEW(recnData->credit), date);
    gnc_reconcile_view_commit(GNC_RECONCILE_VIEW(recnData->debit), date);
    acct_traverse_descendants (account, xaccAccountCommitEdit);

    auto_payment = gnc_prefs_get_bool(GNC_PREFS_GROUP_RECONCILE, GNC_PREF_AUTO_CC_PAYMENT);

    xaccAccountClearReconcilePostpone (account);
    xaccAccountSetReconcileLastDate (account, date);

    if (auto_payment &&
            (xaccAccountGetType (account) == ACCT_TYPE_CREDIT) &&
            (gnc_numeric_negative_p (recnData->new_ending)))
    {
        Account *payment_account;
        XferDialog *xfer;

        xfer = gnc_xfer_dialog (GTK_WIDGET (gnc_ui_get_main_window (recnData->window)), account);

        gnc_xfer_dialog_set_amount(xfer, gnc_numeric_neg (recnData->new_ending));

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
recnPostponeCB (GSimpleAction *simple,
                GVariant      *parameter,
                gpointer       user_data)
{
    auto recnData = static_cast<RecnWindow*>(user_data);
    Account *account;

    {
        const char *message = _("Do you want to postpone this reconciliation "
                                "and finish it later?");
        if (!gnc_verify_dialog (GTK_WINDOW (recnData->window), FALSE, "%s", message))
            return;
    }

    gnc_suspend_gui_refresh ();

    recnData->delete_refresh = TRUE;
    account = recn_get_account (recnData);

    acct_traverse_descendants (account, xaccAccountBeginEdit);
    gnc_reconcile_view_postpone (GNC_RECONCILE_VIEW(recnData->credit));
    gnc_reconcile_view_postpone (GNC_RECONCILE_VIEW(recnData->debit));
    acct_traverse_descendants (account, xaccAccountCommitEdit);

    xaccAccountSetReconcilePostponeDate (account, recnData->statement_date);
    xaccAccountSetReconcilePostponeBalance (account, recnData->new_ending);

    gnc_close_gui_component_by_data (WINDOW_RECONCILE_CM_CLASS, recnData);
}


static void
recnCancelCB (GSimpleAction *simple,
              GVariant      *parameter,
              gpointer       user_data)
{
    auto recnData = static_cast<RecnWindow*>(user_data);
    recn_cancel(recnData);
}
