/********************************************************************\
 * dialog-transfer.c -- transfer dialog for GnuCash                 *
 * Copyright (C) 1999 Linas Vepstas                                 *
 * Copyright (C) 2000 Dave Peticolas                                *
 * Copyright (C) 2000 Herbert Thoma                                 *
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

#include <config.h>

#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>
#include <glib/gi18n.h>

#include "dialog-transfer.h"
#include "dialog-utils.h"
#include "gnc-amount-edit.h"
#include "gnc-component-manager.h"
#include "gnc-date-edit.h"
#include "gnc-engine.h"
#include "gnc-euro.h"
#include "gnc-exp-parser.h"
#include "gnc-prefs.h"
#include "gnc-gui-query.h"
#include "gnc-pricedb.h"
#include "gnc-tree-view-account.h"
#include "gnc-ui.h"
#include "Transaction.h"
#include "Account.h"
#include <libguile.h>
#include "swig-runtime.h"
#include "guile-mappings.h"
#include "engine-helpers.h"
#include "gnc-engine-guile.h"
#include "QuickFill.h"
#include <gnc-commodity.h>


#define DIALOG_TRANSFER_CM_CLASS "dialog-transfer"
#define GNC_PREFS_GROUP "dialogs.transfer"

typedef enum
{
    XFER_DIALOG_FROM,
    XFER_DIALOG_TO
} XferDirection;


/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

struct _xferDialog
{
    GtkWidget *dialog;
    GtkWidget *amount_edit;
    GtkWidget *date_entry;
    GtkWidget *num_entry;
    GtkWidget *description_entry;
    GtkWidget *memo_entry;
    GtkWidget *conv_forward;
    GtkWidget *conv_reverse;

    GtkWidget *from_window;
    GtkTreeView * from_tree_view;
    gnc_commodity *from_commodity;
    GtkWidget *to_window;
    GtkTreeView *to_tree_view;
    gnc_commodity *to_commodity;

    QuickFill *qf;     /* Quickfill on transfer descriptions,
                          defaults to matching on the "From" account. */

    XferDirection quickfill;    /* direction match on the account instead. */

    /* stored data for the description quickfill selection function */
    gint desc_start_selection;
    gint desc_end_selection;
    guint desc_selection_source_id;

    GtkWidget *transferinfo_label;

    GtkWidget *from_transfer_label;
    GtkWidget *to_transfer_label;

    GtkWidget *from_currency_label;
    GtkWidget *to_currency_label;

    GtkWidget *from_show_button;
    GtkWidget *to_show_button;

    GtkWidget *curr_xfer_table;

    GtkWidget *price_edit;
    GtkWidget *to_amount_edit;

    GtkWidget *price_radio;
    GtkWidget *amount_radio;

    GtkWidget *fetch_button;

    QofBook *book;
    GNCPriceDB *pricedb;

    /* Where to store the "exchange_rate" at exit (in lieu of
     * creating a transaction)
     */
    gnc_numeric *exch_rate;
    PriceSource price_source;
    const char *price_type;

    /* Callback function to notify of the newly created Transaction */
    gnc_xfer_dialog_cb transaction_cb;
    /* , and its user_data */
    gpointer transaction_user_data;
};

/** Structure passed to "filter tree accounts" function to provide it information */
typedef struct
{
    /** Show income/expense accounts in tree */
    gboolean show_inc_exp;

    /** Show hidden accounts in tree */
    gboolean show_hidden;
} AccountTreeFilterInfo;

static AccountTreeFilterInfo *from_info = NULL;
static AccountTreeFilterInfo *to_info   = NULL;

struct _acct_list_item
{
    char *acct_full_name;
    Account *acct;
};
typedef struct _acct_list_item acct_list_item;


/** Prototypes ***************************************************/
static void gnc_xfer_update_to_amount (XferDialog *xferData);
static void gnc_xfer_dialog_update_conv_info(XferDialog *xferData);

static Account *gnc_transfer_dialog_get_selected_account (XferDialog *dialog,
                                                          XferDirection direction);
static void gnc_transfer_dialog_set_selected_account (XferDialog *dialog,
                                                      Account *account,
                                                      XferDirection direction);

void gnc_xfer_description_insert_cb(GtkEditable *editable,
                                    const gchar *insert_text,
                                    const gint insert_text_len,
                                    gint *start_pos,
                                    XferDialog *xferData);
gboolean gnc_xfer_description_key_press_cb( GtkEntry *entry,
                                            GdkEventKey *event,
                                            XferDialog *xferData );
void gnc_xfer_dialog_fetch (GtkButton *button, XferDialog *xferData);
gboolean gnc_xfer_dialog_inc_exp_filter_func (Account *account,
                                              gpointer data);
void price_amount_radio_toggled_cb(GtkToggleButton *togglebutton, gpointer data);

void gnc_xfer_dialog_response_cb (GtkDialog *dialog, gint response, gpointer data);
void gnc_xfer_dialog_close_cb(GtkDialog *dialog, gpointer data);

/** Implementations **********************************************/

static gnc_numeric
gnc_xfer_dialog_compute_price_value (XferDialog *xferData)
{
    gnc_numeric from_amt, to_amt;
    g_return_val_if_fail (xferData != NULL, gnc_numeric_error (GNC_ERROR_ARG));

    from_amt = gnc_amount_edit_get_amount(GNC_AMOUNT_EDIT(xferData->amount_edit));
    to_amt = gnc_amount_edit_get_amount(GNC_AMOUNT_EDIT(xferData->to_amount_edit));

    return(gnc_numeric_div(to_amt, from_amt, GNC_DENOM_AUTO, GNC_HOW_DENOM_REDUCE));
}

/* Round a price value according to this policy:
 * If both commodities are currencies, round to a fixed denominator.
 * If only one is a currency, round to the currency's scu * a fixed factor.
 * The fixed values are defined in gnc-pricedb.h
 */
static gnc_numeric
round_price(gnc_commodity *from, gnc_commodity *to, gnc_numeric value)
{
    if (gnc_commodity_is_currency(from) && gnc_commodity_is_currency(to))
        value = gnc_numeric_convert(value, CURRENCY_DENOM,
                                    GNC_HOW_RND_ROUND_HALF_UP);
    else if (gnc_commodity_is_currency(to))
    {
        int scu = gnc_commodity_get_fraction (to);
        value = gnc_numeric_convert(value, scu * COMMODITY_DENOM_MULT,
                                    GNC_HOW_RND_ROUND_HALF_UP);
    }
    else if (gnc_commodity_is_currency(from))
    {
        int scu = gnc_commodity_get_fraction (from);
        value = gnc_numeric_convert(value, scu * COMMODITY_DENOM_MULT,
                                    GNC_HOW_RND_ROUND_HALF_UP);
    }
    return value;
}

typedef enum
{
    SAME_DAY,
    NEAREST,
    LATEST
} PriceDate;

typedef struct
{
    GNCPrice *price;
    GNCPriceDB *pricedb;
    gnc_commodity *from;
    gnc_commodity *to;
    time64 time;
    gboolean reverse;
} PriceReq;

static void
price_request_from_xferData(PriceReq *pr, XferDialog *xd)
{
    g_return_if_fail (pr != NULL);
    g_return_if_fail (xd != NULL);
    pr->price = NULL;
    pr->pricedb = xd->pricedb;
    pr->from = xd->from_commodity;
    pr->to = xd->to_commodity;
    pr->time = gnc_date_edit_get_date (GNC_DATE_EDIT (xd->date_entry));
    pr->reverse = FALSE;
}

static gboolean
lookup_price(PriceReq *pr, PriceDate pd)
{
    GNCPrice *prc = NULL;
    g_return_val_if_fail (pr != NULL, FALSE);
    g_return_val_if_fail (pr->pricedb != NULL, FALSE);
    g_return_val_if_fail (pr->from != NULL, FALSE);
    g_return_val_if_fail (pr->to != NULL, FALSE);

    pr->reverse = FALSE;
    switch (pd)
    {
        default:
        case SAME_DAY:
            prc = gnc_pricedb_lookup_day_t64 (pr->pricedb, pr->from,
                                              pr->to, pr->time);
            break;
        case NEAREST:
            prc = gnc_pricedb_lookup_nearest_in_time64 (pr->pricedb, pr->from,
                                                          pr->to, pr->time);
            break;
        case LATEST:
            prc = gnc_pricedb_lookup_latest (pr->pricedb, pr->from, pr->to);
            break;
    }

    if (!prc) //no price found
    {
        PINFO("No price Found for %s, %s",
              gnc_commodity_get_mnemonic(pr->from),
              gnc_commodity_get_mnemonic(pr->to));
        pr->price = NULL;
        return FALSE;
    }

    if (gnc_commodity_equiv(gnc_price_get_currency(prc), pr->from))
    {
        pr->reverse = TRUE;
        PINFO("Found reverse price: 1 %s = %f %s",
              gnc_commodity_get_mnemonic(pr->to),
              gnc_numeric_to_double(gnc_price_get_value(prc)),
              gnc_commodity_get_mnemonic(pr->from));
    }
    else
    {
        PINFO("Found price: 1 %s = %f %s",
              gnc_commodity_get_mnemonic(pr->from),
              gnc_numeric_to_double(gnc_price_get_value(prc)),
              gnc_commodity_get_mnemonic(pr->to));
    }
    pr->price = prc;
    return TRUE;
}

/* (maybe) update the price from the pricedb. */
static void
gnc_xfer_dialog_update_price (XferDialog *xferData)
{
    PriceReq pr;
    gnc_numeric price_value;

    if (!xferData) return;
    if (!GNC_IS_COMMODITY (xferData->from_commodity) ||
        !GNC_IS_COMMODITY (xferData->to_commodity)) return;
    if (gnc_commodity_equal (xferData->from_commodity, xferData->to_commodity))
        return;
    if (!xferData->pricedb) return;

    price_request_from_xferData(&pr, xferData);
    if (!lookup_price(&pr, SAME_DAY))
        if (!lookup_price(&pr, NEAREST))
        return;

    /* grab the price from the pricedb */
    price_value = gnc_price_get_value (pr.price);
    if (pr.reverse)
        price_value = gnc_numeric_invert (price_value);
    gnc_price_unref(pr.price);

    /* and set the price entry */
    gnc_xfer_dialog_set_price_edit(xferData, price_value);

    /* And then update the to_amount */
    gnc_xfer_update_to_amount (xferData);
}

static void
gnc_xfer_dialog_toggle_cb(GtkToggleButton *button, gpointer data)
{
    AccountTreeFilterInfo* info;
    GncTreeViewAccount* treeview = GNC_TREE_VIEW_ACCOUNT (data);

    info = g_object_get_data (G_OBJECT(treeview), "filter-info");
    if (info)
    {
        info->show_inc_exp = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(button));
        info->show_hidden = FALSE;

        gnc_tree_view_account_refilter (treeview);
    }
}

static gboolean
gnc_xfer_dialog_key_press_cb (GtkWidget   *widget,
                              GdkEventKey *event,
                              gpointer     unused)
{
    GtkWidget *toplevel;

    if ((event->keyval == GDK_KEY_Return) || (event->keyval == GDK_KEY_KP_Enter))
    {
        toplevel = gtk_widget_get_toplevel (widget);
        if (gtk_widget_is_toplevel(toplevel) && GTK_IS_WINDOW(toplevel))
        {
            gtk_window_activate_default(GTK_WINDOW(toplevel));
            return TRUE;
        }
    }
    return FALSE;
}

static void
gnc_xfer_dialog_set_price_auto (XferDialog *xferData,
                                gboolean currency_active,
                                const gnc_commodity *from_currency,
                                const gnc_commodity *to_currency)
{
    gnc_numeric from_rate;
    gnc_numeric to_rate;
    gnc_numeric price_value;

    if (!currency_active)
    {
        GtkEntry *entry;
        gnc_xfer_dialog_set_price_edit(xferData, gnc_numeric_zero());
        entry = GTK_ENTRY(gnc_amount_edit_gtk_entry
                          (GNC_AMOUNT_EDIT(xferData->price_edit)));
        gtk_entry_set_text(entry, "");

        gnc_xfer_update_to_amount (xferData);

        return;
    }

    if (!gnc_is_euro_currency (from_currency) ||
        !gnc_is_euro_currency (to_currency))
    {
        gnc_xfer_dialog_update_price (xferData);
        return;
    }

    from_rate = gnc_euro_currency_get_rate (from_currency);
    to_rate = gnc_euro_currency_get_rate (to_currency);

    if (gnc_numeric_zero_p (from_rate) || gnc_numeric_zero_p (to_rate))
        gnc_xfer_dialog_update_price (xferData);

    price_value = gnc_numeric_div (to_rate, from_rate, GNC_DENOM_AUTO, GNC_HOW_DENOM_REDUCE);

    gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT(xferData->price_edit), price_value);

    gnc_xfer_update_to_amount (xferData);
}

static void
gnc_xfer_dialog_curr_acct_activate(XferDialog *xferData)
{
    Account *to_account;
    Account *from_account;
    gboolean curr_active;

    g_return_if_fail (xferData != NULL);
    from_account =
        gnc_transfer_dialog_get_selected_account (xferData, XFER_DIALOG_FROM);

    to_account =
        gnc_transfer_dialog_get_selected_account (xferData, XFER_DIALOG_TO);

    curr_active = (xferData->exch_rate ||
                   ((from_account != NULL) && (to_account != NULL)))
        && !gnc_commodity_equiv(xferData->from_commodity,
                                xferData->to_commodity);

    gtk_widget_set_sensitive(xferData->curr_xfer_table, curr_active);
    gtk_widget_set_sensitive(xferData->price_edit,
                             curr_active && gtk_toggle_button_get_active
                             (GTK_TOGGLE_BUTTON(xferData->price_radio)));
    gtk_widget_set_sensitive(xferData->to_amount_edit,
                             curr_active && gtk_toggle_button_get_active
                             (GTK_TOGGLE_BUTTON(xferData->amount_radio)));
    gtk_widget_set_sensitive(xferData->price_radio, curr_active);
    gtk_widget_set_sensitive(xferData->amount_radio, curr_active);

    gnc_xfer_dialog_set_price_auto (xferData, curr_active,
                                    xferData->from_commodity, xferData->to_commodity);
    gnc_xfer_dialog_update_conv_info(xferData);

    if (!curr_active)
    {
        GtkEntry *entry;

        gnc_amount_edit_set_amount(GNC_AMOUNT_EDIT(xferData->to_amount_edit),
                                   gnc_numeric_zero ());
        entry = GTK_ENTRY(gnc_amount_edit_gtk_entry
                          (GNC_AMOUNT_EDIT(xferData->to_amount_edit)));
        gtk_entry_set_text(entry, "");
    }
}


void
price_amount_radio_toggled_cb(GtkToggleButton *togglebutton, gpointer data)
{
    XferDialog *xferData = data;
    g_return_if_fail (xferData != NULL);

    gtk_widget_set_sensitive(xferData->price_edit, gtk_toggle_button_get_active
                             (GTK_TOGGLE_BUTTON(xferData->price_radio)));
    gtk_widget_set_sensitive(xferData->to_amount_edit,
                             gtk_toggle_button_get_active
                             (GTK_TOGGLE_BUTTON(xferData->amount_radio)));
}


/* Reload the xferDialog quickfill with the descriptions
 * from the currently selected from account.  Note that this
 * doesn't use the initial account passed into gnc_xfer_dialog,
 * because that's NULL if no account is selected in the main
 * account window tree view.
 */
static void
gnc_xfer_dialog_reload_quickfill( XferDialog *xferData )
{
    GList *splitlist, *node;
    Split *split;
    Transaction *trans;
    Account *account;

    account = gnc_transfer_dialog_get_selected_account (xferData, xferData->quickfill);

    /* get a new QuickFill to use */
    gnc_quickfill_destroy( xferData->qf );
    xferData->qf = gnc_quickfill_new();

    splitlist = xaccAccountGetSplitList( account );

    for ( node = splitlist; node; node = node->next )
    {
        split = node->data;
        trans = xaccSplitGetParent( split );
        gnc_quickfill_insert( xferData->qf,
                              xaccTransGetDescription (trans), QUICKFILL_LIFO);
    }
}


static void
gnc_xfer_dialog_from_tree_selection_changed_cb (GtkTreeSelection *selection,
                                                gpointer data)
{
    XferDialog *xferData = data;
    GNCPrintAmountInfo print_info;
    gnc_commodity *commodity;
    Account *account;

    account = gnc_transfer_dialog_get_selected_account (xferData, XFER_DIALOG_FROM);
    if (!account)
        return;

    commodity = gnc_account_or_default_currency(account, NULL);
    gtk_label_set_text(GTK_LABEL(xferData->from_currency_label),
                       gnc_commodity_get_printname(commodity));

    xferData->from_commodity = commodity;

    print_info = gnc_account_print_info (account, FALSE);
    gnc_amount_edit_set_print_info (GNC_AMOUNT_EDIT (xferData->amount_edit),
                                    print_info);
    gnc_amount_edit_set_fraction (GNC_AMOUNT_EDIT (xferData->amount_edit),
                                  xaccAccountGetCommoditySCU (account));

    gnc_xfer_dialog_curr_acct_activate(xferData);

    /* Reload the xferDialog quickfill if it is based on the from account */
    if (xferData->quickfill == XFER_DIALOG_FROM)
        gnc_xfer_dialog_reload_quickfill(xferData);
}


static void
gnc_xfer_dialog_to_tree_selection_changed_cb (GtkTreeSelection *selection, gpointer data)
{
    XferDialog *xferData = data;
    GNCPrintAmountInfo print_info;
    gnc_commodity *commodity;
    Account *account;

    account = gnc_transfer_dialog_get_selected_account (xferData, XFER_DIALOG_TO);
    if (!account)
        return;

    commodity = xaccAccountGetCommodity(account);
    gtk_label_set_text(GTK_LABEL(xferData->to_currency_label),
                       gnc_commodity_get_printname(commodity));

    xferData->to_commodity = commodity;

    print_info = gnc_account_print_info (account, FALSE);
    gnc_amount_edit_set_print_info (GNC_AMOUNT_EDIT (xferData->to_amount_edit),
                                    print_info);
    gnc_amount_edit_set_fraction (GNC_AMOUNT_EDIT (xferData->to_amount_edit),
                                  xaccAccountGetCommoditySCU (account));

    gnc_xfer_dialog_curr_acct_activate(xferData);

    /* Reload the xferDialog quickfill if it is based on the to account */
    if (xferData->quickfill == XFER_DIALOG_TO)
        gnc_xfer_dialog_reload_quickfill(xferData);
}

gboolean
gnc_xfer_dialog_inc_exp_filter_func (Account *account,
                                     gpointer data)
{
    AccountTreeFilterInfo* info;
    GNCAccountType type;

    info = (AccountTreeFilterInfo*)data;

    if (!info->show_hidden && xaccAccountIsHidden(account))
    {
        return FALSE;
    }

    if (info->show_inc_exp)
    {
        return TRUE;
    }

    type = xaccAccountGetType(account);
    return ((type != ACCT_TYPE_INCOME) && (type != ACCT_TYPE_EXPENSE));
}

static void
gnc_xfer_dialog_fill_tree_view(XferDialog *xferData,
                               XferDirection direction)
{
    GtkTreeView *tree_view;
    const char *show_inc_exp_message = _("Show the income and expense accounts");
    GtkWidget *scroll_win;
    GtkWidget *button;
    GtkTreeSelection *selection;
    gboolean  use_accounting_labels;
    AccountTreeFilterInfo *info;
    GtkBuilder *builder = g_object_get_data (G_OBJECT (xferData->dialog), "builder");

    g_return_if_fail (xferData != NULL);
    use_accounting_labels = gnc_prefs_get_bool(GNC_PREFS_GROUP_GENERAL,
                                               GNC_PREF_ACCOUNTING_LABELS);

    /* In "normal" mode (non accounting terms) the account where the
     * money comes from is displayed on the left side and the account
     * where the money gets transferred to is displayed on the right
     * side. In accounting terms the "from" account is called the
     * "credit" account ("Haben" in german) and the "to" account is
     * called "debit" account ("Soll" in german). Accountants told me
     * that they always want the credit account on the right side
     * and the debit on the left side (like the debit and credit
     * columns in the register window). So reverse from and to account
     * trees when in "accountant" mode. -- Herbert Thoma, 2004-01-18
     */
    if (use_accounting_labels)
    {
        button = GTK_WIDGET(gtk_builder_get_object (builder,
                                                    (direction == XFER_DIALOG_TO) ?
                                                    "left_show_button" : "right_show_button"));
        scroll_win = GTK_WIDGET(gtk_builder_get_object (builder,
                                                        (direction == XFER_DIALOG_TO) ?
                                                        "left_trans_window" : "right_trans_window"));
    }
    else
    {
        button = GTK_WIDGET(gtk_builder_get_object (builder,
                                                    (direction == XFER_DIALOG_TO) ?
                                                    "right_show_button" : "left_show_button"));
        scroll_win = GTK_WIDGET(gtk_builder_get_object (builder,
                                                        (direction == XFER_DIALOG_TO) ?
                                                        "right_trans_window" : "left_trans_window"));
    }


    if (direction == XFER_DIALOG_TO)
        info = to_info;
    else
        info = from_info;

    tree_view = GTK_TREE_VIEW(gnc_tree_view_account_new(FALSE));
    gtk_container_add(GTK_CONTAINER(scroll_win), GTK_WIDGET(tree_view));
    info->show_inc_exp = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(button));
    info->show_hidden = FALSE;
    gnc_tree_view_account_set_filter (GNC_TREE_VIEW_ACCOUNT (tree_view),
                                      gnc_xfer_dialog_inc_exp_filter_func,
                                      info,  /* user data */
                                      NULL    /* destroy callback */);
    g_object_set_data (G_OBJECT(tree_view), "filter-info", info);

    gtk_widget_show(GTK_WIDGET(tree_view));
    g_signal_connect (G_OBJECT (tree_view), "key-press-event",
                      G_CALLBACK (gnc_xfer_dialog_key_press_cb), NULL);

    selection = gtk_tree_view_get_selection (tree_view);
    gtk_tree_selection_set_mode (selection, GTK_SELECTION_BROWSE);

    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button), FALSE);
    gtk_widget_set_tooltip_text (button, show_inc_exp_message);

    if (direction == XFER_DIALOG_TO)
    {
        xferData->to_tree_view = tree_view;
        xferData->to_window = scroll_win;
        xferData->to_show_button = GTK_WIDGET (button);
        g_signal_connect (G_OBJECT (selection), "changed",
                          G_CALLBACK (gnc_xfer_dialog_to_tree_selection_changed_cb), xferData);
    }
    else
    {
        xferData->from_tree_view = tree_view;
        xferData->from_window = scroll_win;
        xferData->from_show_button = GTK_WIDGET (button);
        g_signal_connect (G_OBJECT (selection), "changed",
                          G_CALLBACK (gnc_xfer_dialog_from_tree_selection_changed_cb), xferData);
    }
    g_signal_connect (G_OBJECT (button), "toggled",
                      G_CALLBACK (gnc_xfer_dialog_toggle_cb), tree_view);
}


static void
gnc_parse_error_dialog (XferDialog *xferData, const char *error_string)
{
    const char * parse_error_string;
    g_return_if_fail (xferData != NULL);

    parse_error_string = gnc_exp_parser_error_string ();
    if (parse_error_string == NULL)
        parse_error_string = "";

    if (error_string == NULL)
        error_string = "";

    gnc_error_dialog (GTK_WINDOW (xferData->dialog),
                      "%s\n\n%s: %s.",
                      error_string, _("Error"),
                      parse_error_string);
}

/*** Callbacks for description quickfill. ***/

/* gnc_xfer_dialog_quickfill will update the fields of the dialog
 * based on the contents of the Description entry.  Returns TRUE
 * if the fields were updated, or FALSE if the fields were already
 * updated or if the Description couldn't be matched and no updates
 * were made.
 */
static gboolean
gnc_xfer_dialog_quickfill( XferDialog *xferData )
{
    const char *desc;
    Account *match_account;  /* the matched text was from this account */
    Split *split;            /* the split to autocomplete from */
    Split *other = NULL;     /* the other split of the transaction */
    Account *other_acct = NULL;   /* the Account of the other split */
    gboolean changed = FALSE;

    ENTER("xferData=%p", xferData);
    if ( !xferData )
    {
        LEAVE("bad args");
        return( FALSE );
    }

    match_account = gnc_transfer_dialog_get_selected_account (xferData, xferData->quickfill);

    desc = gtk_entry_get_text( GTK_ENTRY(xferData->description_entry) );

    if ( !desc || desc[0] == '\0' )  /* no description to match */
        return( FALSE );

    split = xaccAccountFindSplitByDesc( match_account, desc );

    if ( !split )
    {
        LEAVE("split not found");
        return( FALSE );
    }
    DEBUG("split=%p", split);

    /* Now update any blank fields of the transfer dialog with
     * the memo and amount from the split, and the description
     * we were passed (assumed to match the split's transaction).
     */

    if ( gnc_numeric_zero_p(
             gnc_amount_edit_get_amount(GNC_AMOUNT_EDIT(xferData->amount_edit))))
    {
        gnc_numeric amt;
        DEBUG("updating amount");
        amt = xaccSplitGetValue( split );

        /* If we've matched a previous transfer, it will appear
         * to be negative in the from account.
         * Need to swap the sign in order for this value
         * to be posted as a withdrawal from the "from" account.
         */
        if ( gnc_numeric_negative_p( amt ) )
            amt = gnc_numeric_neg( amt );

        gnc_amount_edit_set_amount( GNC_AMOUNT_EDIT(xferData->amount_edit), amt );
        changed = TRUE;
    }

    if ( !g_strcmp0(gtk_entry_get_text(GTK_ENTRY(xferData->memo_entry)), "" ))
    {
        DEBUG("updating memo");
        gtk_entry_set_text( GTK_ENTRY(xferData->memo_entry),
                            xaccSplitGetMemo( split ) );
        changed = TRUE;
    }

    /* Since we're quickfilling off of one account (either from or to)
     * that account must be the account of the matched split.
     * Find the other account from the other split,
     * and select that account in the appropriate account tree.
     */
    if ( ( other = xaccSplitGetOtherSplit( split ) ) &&
         ( other_acct = xaccSplitGetAccount( other ) ) )
    {
        GNCAccountType other_type;
        GtkWidget *other_button;
        XferDirection other_direction;

        DEBUG("updating other split");
        if (xferData->quickfill == XFER_DIALOG_FROM)
        {
            other_button = xferData->to_show_button;
            other_direction = XFER_DIALOG_TO;
        }
        else
        {
            other_button = xferData->from_show_button;
            other_direction = XFER_DIALOG_FROM;
        }

        other_type = xaccAccountGetType(other_acct);

        /* Don't want to deactivate the button just because this
         * isn't an income or expense account
         */
        if ( (other_type == ACCT_TYPE_EXPENSE) || (other_type == ACCT_TYPE_INCOME) )
            gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(other_button), TRUE);

        gnc_transfer_dialog_set_selected_account (xferData, other_acct, other_direction);

        changed = TRUE;
    }

    return( changed );
}

static gboolean
idle_select_region(gpointer data)
{
    XferDialog *xferData = data;
    g_return_val_if_fail(xferData, FALSE);

    gtk_editable_select_region(GTK_EDITABLE(xferData->description_entry),
                               xferData->desc_start_selection,
                               xferData->desc_end_selection);

    xferData->desc_selection_source_id = 0;
    return FALSE;
}

/* The insert_cb will do the insert and quickfill if possible and set the
 * cursor position accordingly.  It will not set the selection but will register
 * idle_select_region to do that once the program returns to its main loop.
 */
void
gnc_xfer_description_insert_cb(GtkEditable *editable,
                               const gchar *insert_text,
                               const gint insert_text_len,
                               gint *start_pos,
                               XferDialog *xferData)
{
    gchar *prefix, *suffix, *new_text;
    QuickFill *match;
    const gchar *match_str;
    gint prefix_len, new_text_len, match_str_len;

    g_return_if_fail (xferData != NULL);

    if (insert_text_len <= 0)
        return;

    suffix = gtk_editable_get_chars(editable, *start_pos, -1);

    /* If we are inserting in the middle, do nothing */
    if (*suffix)
    {
        g_free(suffix);
        return;
    }
    g_free(suffix);

    prefix = gtk_editable_get_chars(editable, 0, *start_pos);
    new_text = g_strconcat(prefix, insert_text, (gchar*) NULL);
    prefix_len = strlen(prefix);
    new_text_len = prefix_len + insert_text_len;
    g_free(prefix);

    if ((match = gnc_quickfill_get_string_match(xferData->qf, new_text))
        && (match_str = gnc_quickfill_string(match))
        && ((match_str_len = strlen(match_str)) > new_text_len))
    {
        g_signal_handlers_block_matched (G_OBJECT (editable),
                                         G_SIGNAL_MATCH_DATA, 0, 0, NULL, NULL, xferData);

        gtk_editable_insert_text(editable,
                                 match_str + prefix_len,
                                 match_str_len - prefix_len,
                                 start_pos);

        g_signal_handlers_unblock_matched (G_OBJECT (editable),
                                           G_SIGNAL_MATCH_DATA, 0, 0, NULL, NULL, xferData);

        /* stop the current insert */
        g_signal_stop_emission_by_name (G_OBJECT (editable), "insert_text");

        /* set the position */
        *start_pos = g_utf8_strlen(new_text, -1);

        /* select region on idle, because it would be reset once this function
           finishes */
        xferData->desc_start_selection = *start_pos;
        xferData->desc_end_selection = -1;
        xferData->desc_selection_source_id = g_idle_add(idle_select_region,
                                                        xferData);
    }
    g_free(new_text);
}

gboolean
gnc_xfer_description_key_press_cb( GtkEntry *entry,
                                   GdkEventKey *event,
                                   XferDialog *xferData )
{
    gboolean done_with_input = FALSE;

    /* Most "special" keys are allowed to be handled directly by
     * the entry's key press handler, but in some cases that doesn't
     * seem to work right, so handle them here.
     */
    ENTER(" ");
    switch ( event->keyval )
    {
        case GDK_KEY_Return:
        case GDK_KEY_KP_Enter:
            gnc_xfer_dialog_quickfill( xferData );
            /* NOT done with input, activate the default button of the dialog. */
            break;

        case GDK_KEY_Tab:
        case GDK_KEY_ISO_Left_Tab:
            if ( !( event->state & GDK_SHIFT_MASK) )    /* Complete on Tab,
                                                         * but not Shift-Tab */
            {
                gnc_xfer_dialog_quickfill( xferData );
                /* NOT done with input, though, since we need to focus to the next
                 * field.  Unselect the current field, though.
                 */
                gtk_editable_select_region( GTK_EDITABLE(xferData->description_entry),
                                            0, 0 );
            }
            break;
    }

    LEAVE("done=%d", done_with_input);
    return( done_with_input );
}

/*** End of quickfill-specific callbacks ***/

static void
gnc_xfer_dialog_update_conv_info (XferDialog *xferData)
{
    const gchar *to_mnemonic, *from_mnemonic;
    gchar *string;
    gnc_numeric rate;

    from_mnemonic = gnc_commodity_get_mnemonic(xferData->from_commodity);
    to_mnemonic = gnc_commodity_get_mnemonic(xferData->to_commodity);

    /* On the theory that if we don't have a mnemonic then we don't
     * have a commodity...  On Solaris this crashes without a string.
     * So, just leave now and wait for the second initialization to
     * occur.
     */
    if (!from_mnemonic || !to_mnemonic)
        return;

    rate = gnc_amount_edit_get_amount(GNC_AMOUNT_EDIT(xferData->price_edit));
    if (gnc_numeric_zero_p(rate))
    {
        string = g_strdup_printf("1 %s = x %s", from_mnemonic, to_mnemonic);
        gtk_label_set_text(GTK_LABEL(xferData->conv_forward), string);
        g_free(string);

        string = g_strdup_printf("1 %s = x %s", to_mnemonic, from_mnemonic);
        gtk_label_set_text(GTK_LABEL(xferData->conv_reverse), string);
        g_free(string);
    }
    else
    {
        string = g_strdup_printf("1 %s = %f %s", from_mnemonic,
                                 gnc_numeric_to_double(rate), to_mnemonic);
        gtk_label_set_text(GTK_LABEL(xferData->conv_forward), string);
        g_free(string);

        rate = gnc_numeric_invert(rate);
        string = g_strdup_printf("1 %s = %f %s", to_mnemonic,
                                 gnc_numeric_to_double(rate), from_mnemonic);
        gtk_label_set_text(GTK_LABEL(xferData->conv_reverse), string);
        g_free(string);
    }
}

static gboolean
gnc_xfer_amount_update_cb(GtkWidget *widget, GdkEventFocus *event,
                          gpointer data)
{
    XferDialog * xferData = data;
    g_return_val_if_fail (xferData != NULL, FALSE);

    gnc_amount_edit_evaluate (GNC_AMOUNT_EDIT (xferData->amount_edit));

    gnc_xfer_update_to_amount (xferData);

    return FALSE;
}


static void
gnc_xfer_update_to_amount (XferDialog *xferData)
{
    GNCAmountEdit *amount_edit, *price_edit, *to_amount_edit;
    gnc_numeric price_value, to_amount;
    Account *account;
    int scu = 0;

    g_return_if_fail(xferData);

    xferData->price_source = PRICE_SOURCE_USER_PRICE;

    /* Get the amount editing controls of the dialog. */
    amount_edit     = GNC_AMOUNT_EDIT(xferData->amount_edit);
    price_edit      = GNC_AMOUNT_EDIT(xferData->price_edit);
    to_amount_edit  = GNC_AMOUNT_EDIT(xferData->to_amount_edit);

    /* Determine the SCU (smallest commodity unit) of the "to" amount. */
    account = gnc_transfer_dialog_get_selected_account(xferData, XFER_DIALOG_TO);
    if (account == NULL)
        account = gnc_transfer_dialog_get_selected_account(xferData,
                                                           XFER_DIALOG_FROM);
    if (account != NULL)
        scu = xaccAccountGetCommoditySCU(account);
    else if (xferData->to_commodity != NULL)
        scu = gnc_commodity_get_fraction(xferData->to_commodity);

    /* Determine the amount to transfer. */
    if (!gnc_amount_edit_evaluate(price_edit) ||
        gnc_numeric_zero_p(price_value = gnc_amount_edit_get_amount(price_edit)))
        to_amount = gnc_numeric_zero();
    else
        to_amount = gnc_numeric_mul(gnc_amount_edit_get_amount(amount_edit),
                                    price_value, scu, GNC_HOW_RND_ROUND_HALF_UP);

    /* Update the dialog. */
    gnc_amount_edit_set_amount(to_amount_edit, to_amount);
    if (gnc_numeric_zero_p(to_amount))
        gtk_entry_set_text(GTK_ENTRY(gnc_amount_edit_gtk_entry(to_amount_edit)),
                           "");

    gnc_xfer_dialog_update_conv_info(xferData);
}


static gboolean
gnc_xfer_price_update_cb(GtkWidget *widget, GdkEventFocus *event,
                         gpointer data)
{
    XferDialog *xferData = data;

    gnc_xfer_update_to_amount (xferData);
    xferData->price_type = PRICE_TYPE_TRN;


    return FALSE;
}

static gboolean
gnc_xfer_date_changed_cb(GtkWidget *widget, gpointer data)
{
    XferDialog *xferData = data;

    if (xferData)
        gnc_xfer_dialog_update_price (xferData);

    return FALSE;
}

static gboolean
gnc_xfer_to_amount_update_cb(GtkWidget *widget, GdkEventFocus *event,
                             gpointer data)
{
    XferDialog *xferData = data;
    gnc_numeric price_value;

    gnc_amount_edit_evaluate (GNC_AMOUNT_EDIT (xferData->to_amount_edit));
    price_value = gnc_xfer_dialog_compute_price_value(xferData);
    gnc_amount_edit_set_amount(GNC_AMOUNT_EDIT(xferData->price_edit),
                               price_value);
    xferData->price_source = PRICE_SOURCE_XFER_DLG_VAL;
    xferData->price_type = PRICE_TYPE_TRN;
    gnc_xfer_dialog_update_conv_info(xferData);

    return FALSE;
}


/********************************************************************\
 * gnc_xfer_dialog_select_from_account                              *
 *   select the from account in a xfer dialog                       *
 *                                                                  *
 * Args:   xferData - xfer dialog structure                         *
 *         account  - account to select                             *
 * Return: none                                                     *
\********************************************************************/
void
gnc_xfer_dialog_select_from_account(XferDialog *xferData, Account *account)
{
    gnc_transfer_dialog_set_selected_account (xferData, account, XFER_DIALOG_FROM);
}


/********************************************************************\
 * gnc_xfer_dialog_select_to_account                                *
 *   select the to account in a xfer dialog                         *
 *                                                                  *
 * Args:   xferData - xfer dialog structure                         *
 *         account  - account to select                             *
 * Return: none                                                     *
\********************************************************************/
void
gnc_xfer_dialog_select_to_account(XferDialog *xferData, Account *account)
{
    gnc_transfer_dialog_set_selected_account (xferData, account, XFER_DIALOG_TO);
}

void
gnc_xfer_dialog_select_from_currency(XferDialog *xferData, gnc_commodity *cur)
{
    if (!xferData) return;
    if (!cur) return;

    gtk_label_set_text(GTK_LABEL(xferData->from_currency_label),
                       gnc_commodity_get_printname(cur));

    gnc_amount_edit_set_print_info(GNC_AMOUNT_EDIT(xferData->amount_edit),
                                   gnc_commodity_print_info(cur, FALSE));
    gnc_amount_edit_set_fraction(GNC_AMOUNT_EDIT(xferData->amount_edit),
                                 gnc_commodity_get_fraction (cur));

    xferData->from_commodity = cur;
    gnc_xfer_dialog_curr_acct_activate(xferData);
}

void
gnc_xfer_dialog_select_to_currency(XferDialog *xferData, gnc_commodity *cur)
{
    g_return_if_fail (cur && GNC_IS_COMMODITY (cur));
    gtk_label_set_text(GTK_LABEL(xferData->to_currency_label),
                       gnc_commodity_get_printname(cur));

    gnc_amount_edit_set_print_info(GNC_AMOUNT_EDIT(xferData->to_amount_edit),
                                   gnc_commodity_print_info(cur, FALSE));
    gnc_amount_edit_set_fraction(GNC_AMOUNT_EDIT(xferData->to_amount_edit),
                                 gnc_commodity_get_fraction(cur));

    xferData->to_commodity = cur;
    gnc_xfer_dialog_curr_acct_activate(xferData);
}

static void
gnc_xfer_dialog_lock_account_tree(XferDialog *xferData,
                                  XferDirection direction,
                                  gboolean hide)
{
    GtkTreeView *tree_view;
    GtkWidget *show_button;
    GtkWidget *scroll_win;

    if (xferData == NULL)
        return;

    switch (direction)
    {
        case XFER_DIALOG_FROM:
            tree_view = xferData->from_tree_view;
            scroll_win = xferData->from_window;
            show_button = xferData->from_show_button;
            break;
        case XFER_DIALOG_TO:
            tree_view = xferData->to_tree_view;
            scroll_win = xferData->to_window;
            show_button = xferData->to_show_button;
            break;
        default:
            return;
    }

    gtk_widget_set_sensitive( GTK_WIDGET(tree_view), FALSE );
    gtk_widget_set_sensitive( GTK_WIDGET(show_button), FALSE );

    if (hide)
    {
        gtk_widget_hide( scroll_win );
        gtk_widget_hide( GTK_WIDGET(show_button) );
    }
}


/********************************************************************\
 * gnc_xfer_dialog_lock_from_account_tree                           *
 *   prevent changes to the from account tree in an xfer dialog     *
 *                                                                  *
 * Args:   xferData - xfer dialog structure                         *
 * Return: none                                                     *
\********************************************************************/
void
gnc_xfer_dialog_lock_from_account_tree(XferDialog *xferData)
{
    gnc_xfer_dialog_lock_account_tree(xferData, XFER_DIALOG_FROM, FALSE);
}


/********************************************************************\
 * gnc_xfer_dialog_lock_to_account_tree                             *
 *   prevent changes to the to account tree in an xfer dialog       *
 *                                                                  *
 * Args:   xferData - xfer dialog structure                         *
 * Return: none                                                     *
\********************************************************************/
void
gnc_xfer_dialog_lock_to_account_tree(XferDialog *xferData)
{
    gnc_xfer_dialog_lock_account_tree(xferData, XFER_DIALOG_TO, FALSE);
}


/********************************************************************\
 * gnc_xfer_dialog_hide_from_account_tree                           *
 *   prevent changes to the from account tree in an xfer dialog     *
 *                                                                  *
 * Args:   xferData - xfer dialog structure                         *
 * Return: none                                                     *
\********************************************************************/
void
gnc_xfer_dialog_hide_from_account_tree(XferDialog *xferData)
{
    gnc_xfer_dialog_lock_account_tree(xferData, XFER_DIALOG_FROM, TRUE);
}


/********************************************************************\
 * gnc_xfer_dialog_hide_to_account_tree                             *
 *   prevent changes to the to account tree in an xfer dialog       *
 *                                                                  *
 * Args:   xferData - xfer dialog structure                         *
 * Return: none                                                     *
\********************************************************************/
void
gnc_xfer_dialog_hide_to_account_tree(XferDialog *xferData)
{
    gnc_xfer_dialog_lock_account_tree(xferData, XFER_DIALOG_TO, TRUE);
}


/********************************************************************\
 * gnc_xfer_dialog_is_exchange_dialog                               *
 *   set the dialog as an "exchange-dialog", which means that the   *
 *   Transfer Information table is read-only (and the dialog        *
 *   will NOT create a transaction when it is closed)               *
 *                                                                  *
 * Args:   xferData - xfer dialog structure                         *
 *         exch_rate - place to store the exchange rate at exit     *
 * Return: none                                                     *
\********************************************************************/
void
gnc_xfer_dialog_is_exchange_dialog (XferDialog *xferData,
                                    gnc_numeric *exch_rate)
{
    GNCAmountEdit *gae;

    g_return_if_fail(xferData);
    ENTER("xferData=%p, exch_rate=%p (%s)", xferData, exch_rate,
          exch_rate == NULL ? "NULL" : xaccPrintAmount(*exch_rate,
                                                       gnc_default_print_info(FALSE)));

    gtk_widget_set_sensitive (xferData->amount_edit, FALSE);
    gtk_widget_set_sensitive (xferData->date_entry, FALSE);
    gtk_widget_set_sensitive (xferData->num_entry, FALSE);
    gtk_widget_set_sensitive (xferData->description_entry, FALSE);
    gtk_widget_set_sensitive (xferData->memo_entry, FALSE);


    gae = GNC_AMOUNT_EDIT (xferData->price_edit);
    gtk_widget_grab_focus (gnc_amount_edit_gtk_entry (gae));

    xferData->exch_rate = exch_rate;

    LEAVE(" ");
}

/********************************************************************\
 * gnc_xfer_dialog_set_amount                                       *
 *   set the amount in the given xfer dialog                        *
 *                                                                  *
 * Args:   xferData - xfer dialog structure                         *
 *         amount   - the amount to set                             *
 * Return: none                                                     *
\********************************************************************/
void
gnc_xfer_dialog_set_amount(XferDialog *xferData, gnc_numeric amount)
{
    Account * account;

    if (xferData == NULL)
        return;

    account = gnc_transfer_dialog_get_selected_account (xferData,
                                                        XFER_DIALOG_FROM);
    if (account == NULL)
        gnc_transfer_dialog_get_selected_account (xferData, XFER_DIALOG_TO);

    gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (xferData->amount_edit), amount);
}
void gnc_xfer_dialog_set_amount_sensitive(XferDialog *xferData,
                                          gboolean is_sensitive)
{
    g_assert(xferData);
    gtk_widget_set_sensitive(gnc_amount_edit_gtk_entry(GNC_AMOUNT_EDIT (xferData->amount_edit)), is_sensitive);
}

static void
gnc_xfer_dialog_set_fetch_sensitive (GtkWidget *fetch)
{
    if (gnc_quote_source_fq_installed ())
    {
        gtk_widget_set_sensitive (fetch, TRUE);
        gtk_widget_set_tooltip_text (fetch, _("Retrieve the current online quote. This will fail if there is a manually-created price for today."));
        return;
    }
    gtk_widget_set_sensitive (fetch, FALSE);
    gtk_widget_set_tooltip_text (fetch, _("Finance::Quote must be installed to enable this button."));
    return;
}

/********************************************************************\
 * gnc_xfer_dialog_set_description                                  *
 *   set the description in the given xfer dialog                   *
 *                                                                  *
 * Args:   xferData    - xfer dialog structure                      *
 *         description - the description to set                     *
 * Return: none                                                     *
\********************************************************************/
void
gnc_xfer_dialog_set_description(XferDialog *xferData, const char *description)
{
    if (xferData == NULL)
        return;

    gtk_entry_set_text(GTK_ENTRY(xferData->description_entry), description);
    gnc_quickfill_insert( xferData->qf, description, QUICKFILL_LIFO );
}

/********************************************************************\
 * gnc_xfer_dialog_set_memo                                         *
 *   set the memo in the given xfer dialog                          *
 *                                                                  *
 * Args:   xferData    - xfer dialog structure                      *
 *         memo        - the memo to set                            *
 * Return: none                                                     *
\********************************************************************/
void
gnc_xfer_dialog_set_memo(XferDialog *xferData, const char *memo)
{
    if (xferData == NULL)
        return;

    gtk_entry_set_text(GTK_ENTRY(xferData->memo_entry), memo);
    /* gnc_quickfill_insert( xferData->qf, memo, QUICKFILL_LIFO ); */
}

/********************************************************************\
 * gnc_xfer_dialog_set_num                                          *
 *   set the num in the given xfer dialog                           *
 *                                                                  *
 * Args:   xferData    - xfer dialog structure                      *
 *         num        - the num to set                              *
 * Return: none                                                     *
\********************************************************************/
void
gnc_xfer_dialog_set_num(XferDialog *xferData, const char *num)
{
    if (xferData == NULL)
        return;

    gtk_entry_set_text(GTK_ENTRY(xferData->num_entry), num);
    /* gnc_quickfill_insert( xferData->qf, num, QUICKFILL_LIFO ); */
}

/********************************************************************\
 * gnc_xfer_dialog_set_date                                         *
 *   set the date in the given xfer dialog                          *
 *                                                                  *
 * Args:   xferData    - xfer dialog structure                      *
 *         set_date    - the date to set                            *
 * Return: none                                                     *
\********************************************************************/
void
gnc_xfer_dialog_set_date(XferDialog *xferData, time64 set_date)
{
    if (xferData == NULL)
        return;

    gnc_date_edit_set_time( GNC_DATE_EDIT(xferData->date_entry), set_date );
}
void gnc_xfer_dialog_set_date_sensitive(XferDialog *xferData,
                                        gboolean is_sensitive)
{
    g_assert(xferData);
    gtk_widget_set_sensitive (xferData->date_entry, is_sensitive);
}

void
gnc_xfer_dialog_set_price_edit(XferDialog *xferData, gnc_numeric price_value)
{
    if (xferData == NULL)
        return;

    if (gnc_numeric_zero_p (price_value))
        return;

    gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (xferData->price_edit),
                                price_value);

    gnc_xfer_update_to_amount (xferData);
}

static gboolean
check_accounts  (XferDialog* xferData, Account* from_account,
                 Account* to_account)
{
    if ((from_account == NULL) || (to_account == NULL))
    {
        const char *message = _("You must specify an account to transfer from, "
                                "or to, or both, for this transaction. "
                                "Otherwise, it will not be recorded.");
        gnc_error_dialog (GTK_WINDOW (xferData->dialog), "%s", message);
        LEAVE("bad account");
        return FALSE;
    }

    if (from_account == to_account)
    {
        const char *message = _("You can't transfer from and to the same "
                                "account!");
        gnc_error_dialog (GTK_WINDOW (xferData->dialog), "%s", message);
        LEAVE("same account");
        return FALSE;
    }

    if (xaccAccountGetPlaceholder(from_account) ||
        xaccAccountGetPlaceholder(to_account))
    {
        const char *placeholder_format =
            _("The account %s does not allow transactions.");
        char *name;

        if (xaccAccountGetPlaceholder(from_account))
            name = gnc_account_get_full_name(from_account);
        else
            name = gnc_account_get_full_name(to_account);
        gnc_error_dialog (GTK_WINDOW (xferData->dialog), placeholder_format, name);
        g_free(name);
        LEAVE("placeholder");
        return FALSE;
    }

    if (!gnc_commodity_is_iso (xferData->from_commodity))
    {
        const char *message =
            _("You can't transfer from a non-currency account. "
              "Try reversing the \"from\" and \"to\" accounts "
              "and making the \"amount\" negative.");
        gnc_error_dialog (GTK_WINDOW (xferData->dialog), "%s", message);
        LEAVE("non-currency");
        return FALSE;
    }
    return TRUE;
}

static gboolean
check_edit(XferDialog *xferData)
{
    if (!gnc_amount_edit_evaluate (GNC_AMOUNT_EDIT (xferData->price_edit)))
    {
        if (gtk_toggle_button_get_active
            (GTK_TOGGLE_BUTTON(xferData->price_radio)))
        {
            gnc_parse_error_dialog (xferData, _("You must enter a valid price."));
            LEAVE("invalid price");
            return FALSE;
        }
    }

    if (!gnc_amount_edit_evaluate (GNC_AMOUNT_EDIT (xferData->to_amount_edit)))
    {
        if (gtk_toggle_button_get_active
            (GTK_TOGGLE_BUTTON(xferData->amount_radio)))
        {
            gnc_parse_error_dialog (xferData,
                                    _("You must enter a valid `to' amount."));
            LEAVE("invalid to amount");
            return FALSE;
        }
    }
    return TRUE;
}

static void
create_transaction(XferDialog *xferData, time64 time,
                   Account *from_account, Account* to_account,
                   gnc_numeric amount, gnc_numeric to_amount)
{
    Transaction *trans;
    Split *from_split;
    Split *to_split;
    const char *string;
    /* Create the transaction */
    trans = xaccMallocTransaction(xferData->book);

    xaccTransBeginEdit(trans);

    xaccTransSetCurrency(trans, xferData->from_commodity);
    xaccTransSetDatePostedSecs(trans, time);

    /* Trans-Num or Split-Action set with gnc_set_num_action below per book
     * option */

    string = gtk_entry_get_text(GTK_ENTRY(xferData->description_entry));
    xaccTransSetDescription(trans, string);

    /* create from split */
    from_split = xaccMallocSplit(xferData->book);
    xaccTransAppendSplit(trans, from_split);

    /* create to split */
    to_split = xaccMallocSplit(xferData->book);
    xaccTransAppendSplit(trans, to_split);

    xaccAccountBeginEdit(from_account);
    xaccAccountInsertSplit(from_account, from_split);

    xaccAccountBeginEdit(to_account);
    xaccAccountInsertSplit(to_account, to_split);

    xaccSplitSetBaseValue(from_split, gnc_numeric_neg (amount),
                          xferData->from_commodity);
    xaccSplitSetBaseValue(to_split, amount, xferData->from_commodity);
    xaccSplitSetBaseValue(to_split, to_amount, xferData->to_commodity);

    /* Set the transaction number or split action field based on book option*/
    string = gtk_entry_get_text(GTK_ENTRY(xferData->num_entry));
    gnc_set_num_action (trans, from_split, string, NULL);

    /* Set the memo fields */
    string = gtk_entry_get_text(GTK_ENTRY(xferData->memo_entry));
    xaccSplitSetMemo(from_split, string);
    xaccSplitSetMemo(to_split, string);

    /* finish transaction */
    xaccTransCommitEdit(trans);
    xaccAccountCommitEdit(from_account);
    xaccAccountCommitEdit(to_account);

    /* If there is a registered callback handler that should be
       notified of the newly created Transaction, call it now. */
    if (xferData->transaction_cb)
        xferData->transaction_cb(trans, xferData->transaction_user_data);
}

static gnc_numeric
swap_commodities(gnc_commodity **from, gnc_commodity **to, gnc_numeric value)
{
    gnc_commodity *tmp = *to;

    *to = *from;
    *from = tmp;
    value = gnc_numeric_invert(value);
    return value;
}

static void
update_price(XferDialog *xferData, PriceReq *pr)
{
    gnc_commodity *from = xferData->from_commodity;
    gnc_commodity *to = xferData->to_commodity;
    gnc_numeric value = gnc_amount_edit_get_amount(GNC_AMOUNT_EDIT(xferData->price_edit));
    gnc_numeric price_value = gnc_price_get_value(pr->price);
    gnc_numeric rounded_pr_value = round_price(pr->from, pr->to, price_value);
    gnc_numeric rounded_value;

    if (gnc_price_get_source(pr->price) < xferData->price_source)
    {
        PINFO("Existing price is preferred, so won't supersede.");
        gnc_price_unref (pr->price);
        return;
    }

    if (pr->reverse)
        value = swap_commodities(&from, &to, value);
    /* Test the rounded values for equality to minimize price-dithering. */
    rounded_value = round_price(from, to, value);
    if (gnc_numeric_equal(rounded_value, rounded_pr_value))
    {
        PINFO("Same price for %s in %s",
              gnc_commodity_get_mnemonic(pr->from),
              gnc_commodity_get_mnemonic(pr->to));
        gnc_price_unref (pr->price);
        return;
    }
    gnc_price_begin_edit (pr->price);
    gnc_price_set_time64 (pr->price, pr->time);
    gnc_price_set_typestr(pr->price, xferData->price_type);
    gnc_price_set_value (pr->price, value);
    gnc_price_commit_edit (pr->price);
    PINFO("Updated price: 1 %s = %f %s",
          gnc_commodity_get_mnemonic(pr->from),
          gnc_numeric_to_double(gnc_price_get_value(pr->price)),
          gnc_commodity_get_mnemonic(pr->to));
    gnc_price_unref (pr->price);
}

static void
new_price(XferDialog *xferData, time64 time)
{
    GNCPrice *price = NULL;
    gnc_commodity *from = xferData->from_commodity;
    gnc_commodity *to = xferData->to_commodity;
    gnc_numeric value = gnc_amount_edit_get_amount(GNC_AMOUNT_EDIT(xferData->price_edit));

    value = gnc_numeric_abs(value);

    /* store price against the non currency commodity */
    if (gnc_commodity_is_currency (from)  && !gnc_commodity_is_currency (to))
        value = swap_commodities (&from, &to, value);
    /* store rate against default currency if present */
    else if (from == gnc_default_currency() && to != gnc_default_currency())
        value = swap_commodities (&from, &to, value);

    value = round_price (from, to, value);
    price = gnc_price_create (xferData->book);
    gnc_price_begin_edit (price);
    gnc_price_set_commodity (price, from);
    gnc_price_set_currency (price, to);
    gnc_price_set_time64 (price, time);
    gnc_price_set_source (price, xferData->price_source);
    gnc_price_set_typestr (price, xferData->price_type);
    gnc_price_set_value (price, value);
    gnc_pricedb_add_price (xferData->pricedb, price);
    gnc_price_commit_edit (price);
    PINFO("Created price: 1 %s = %f %s", gnc_commodity_get_mnemonic(from),
          gnc_numeric_to_double(value), gnc_commodity_get_mnemonic(to));
    gnc_price_unref (price);
}

static void
create_price(XferDialog *xferData, time64 time)
{
    PriceReq pr;

/* Bail in the unlikely event that both currencies have joined the Euro. */
    if (gnc_is_euro_currency (xferData->from_commodity) &&
        gnc_is_euro_currency (xferData->to_commodity))
        return;

    price_request_from_xferData(&pr, xferData);
    if (lookup_price(&pr, SAME_DAY))
    {
        update_price(xferData, &pr);
        return;
    }
    new_price (xferData, time);
}

void
gnc_xfer_dialog_response_cb (GtkDialog *dialog, gint response, gpointer data)
{
    XferDialog *xferData = data;
    Account *to_account;
    Account *from_account;
    gnc_numeric amount, to_amount;
    time64 time;
    GDate date;

    g_return_if_fail (xferData != NULL);
    ENTER(" ");

    if (response == GTK_RESPONSE_APPLY)
    {
        LEAVE("fetching exchange rate");
        return;
    }

    /* We're closing, either by cancel, esc or ok
     * Remove date changed handler to prevent it from triggering
     * on a focus-out event while we're already destroying the widget */
    g_signal_handlers_disconnect_by_func (G_OBJECT (xferData->date_entry),
                                            G_CALLBACK (gnc_xfer_date_changed_cb),
                                            xferData);

    if (response != GTK_RESPONSE_OK)
    {
        gnc_close_gui_component_by_data (DIALOG_TRANSFER_CM_CLASS, xferData);
        LEAVE("cancel, etc.");
        return;
    }

    from_account = gnc_transfer_dialog_get_selected_account (xferData, XFER_DIALOG_FROM);
    to_account = gnc_transfer_dialog_get_selected_account (xferData, XFER_DIALOG_TO);

    if (xferData->exch_rate == NULL &&
        !check_accounts(xferData, from_account, to_account))
        return;

    if (!gnc_amount_edit_evaluate (GNC_AMOUNT_EDIT (xferData->amount_edit)))
    {
        gnc_parse_error_dialog (xferData, _("You must enter a valid amount."));
        LEAVE("no amount");
        return;
    }

    amount = gnc_amount_edit_get_amount(GNC_AMOUNT_EDIT(xferData->amount_edit));

    if (gnc_numeric_zero_p (amount))
    {
        const char *message = _("You must enter an amount to transfer.");
        gnc_error_dialog (GTK_WINDOW (xferData->dialog), "%s", message);
        LEAVE("invalid from amount");
        return;
    }
    g_date_clear (&date, 1);
    gnc_date_edit_get_gdate (GNC_DATE_EDIT (xferData->date_entry), &date);
    time = gdate_to_time64 (date);

    if (!gnc_commodity_equiv(xferData->from_commodity, xferData->to_commodity))
    {
        if (!check_edit(xferData))
            return;
        to_amount = gnc_amount_edit_get_amount
            (GNC_AMOUNT_EDIT(xferData->to_amount_edit));
    }
    else
        to_amount = amount;

    gnc_suspend_gui_refresh ();

    if (xferData->exch_rate)
    {
        gnc_numeric price_value;

        /* If we've got the price-button set, then make sure we update the
         * to-amount before we use it.
         */
        if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(xferData->price_radio)))
            gnc_xfer_update_to_amount(xferData);

        price_value = gnc_xfer_dialog_compute_price_value(xferData);
        gnc_amount_edit_set_amount(GNC_AMOUNT_EDIT(xferData->price_edit),
                                   price_value);
        *(xferData->exch_rate) = gnc_numeric_abs(price_value);
    }
    else
        create_transaction (xferData, time, from_account, to_account,
                            amount, to_amount);
    /* try to save this to the pricedb */
    if (xferData->pricedb && !gnc_commodity_equal (xferData->from_commodity,
                                                   xferData->to_commodity))
        create_price(xferData, time);
    /* Refresh everything */
    gnc_resume_gui_refresh ();

    DEBUG("close component");
    gnc_close_gui_component_by_data (DIALOG_TRANSFER_CM_CLASS, xferData);
    LEAVE("ok");
}

void
gnc_xfer_dialog_close_cb(GtkDialog *dialog, gpointer data)
{
    XferDialog * xferData = data;
    GtkWidget *entry;

    /* Notify transaction callback to unregister here */
    if (xferData->transaction_cb)
        xferData->transaction_cb(NULL, xferData->transaction_user_data);

    entry = gnc_amount_edit_gtk_entry(GNC_AMOUNT_EDIT(xferData->amount_edit));
    g_signal_handlers_disconnect_matched (G_OBJECT (entry), G_SIGNAL_MATCH_DATA,
                                          0, 0, NULL, NULL, xferData);

    entry = gnc_amount_edit_gtk_entry(GNC_AMOUNT_EDIT(xferData->price_edit));
    g_signal_handlers_disconnect_matched (G_OBJECT (entry), G_SIGNAL_MATCH_DATA,
                                          0, 0, NULL, NULL, xferData);

    entry = gnc_amount_edit_gtk_entry(GNC_AMOUNT_EDIT(xferData->to_amount_edit));
    g_signal_handlers_disconnect_matched (G_OBJECT (entry), G_SIGNAL_MATCH_DATA,
                                          0, 0, NULL, NULL, xferData);

    entry = xferData->description_entry;
    g_signal_handlers_disconnect_matched (G_OBJECT (entry), G_SIGNAL_MATCH_DATA,
                                          0, 0, NULL, NULL, xferData);

    DEBUG("unregister component");
    gnc_unregister_gui_component_by_data (DIALOG_TRANSFER_CM_CLASS, xferData);

    gnc_quickfill_destroy (xferData->qf);
    xferData->qf = NULL;

    if (xferData->desc_selection_source_id)
        g_source_remove (xferData->desc_selection_source_id);

    g_free(xferData);
    xferData = NULL;

    DEBUG("xfer dialog destroyed");
}


void
gnc_xfer_dialog_fetch (GtkButton *button, XferDialog *xferData)
{
    PriceReq pr;
    SCM quotes_func;
    SCM book_scm;
    SCM scm_window;

    g_return_if_fail (xferData);

    ENTER(" ");

    quotes_func = scm_c_eval_string ("gnc:book-add-quotes");

    if (!scm_is_procedure (quotes_func))
    {
        LEAVE("quote retrieval failed");
        return;
    }

    book_scm = gnc_book_to_scm (xferData->book);
    if (scm_is_true (scm_not (book_scm)))
    {
        LEAVE("no book");
        return;
    }

    scm_window =  SWIG_NewPointerObj(xferData->dialog,
                                     SWIG_TypeQuery("_p_GtkWindow"), 0);

    if (scm_is_true (scm_not (book_scm)))
    {
        LEAVE("no scm window");
        return;
    }

    gnc_set_busy_cursor (NULL, TRUE);
    scm_call_2 (quotes_func, scm_window, book_scm);
    gnc_unset_busy_cursor (NULL);

    /*the results should be in the price db now, but don't crash if not. */
    price_request_from_xferData(&pr, xferData);
    if (lookup_price(&pr, LATEST))
    {
        gnc_numeric price_value = gnc_price_get_value(pr.price);
        if (pr.reverse)
            price_value = gnc_numeric_invert(price_value);
         gnc_xfer_dialog_set_price_edit(xferData, price_value);
        gnc_price_unref (pr.price);
    }

    LEAVE("quote retrieved");

}

static void
gnc_xfer_dialog_create(GtkWidget *parent, XferDialog *xferData)
{
    GtkBuilder *builder;
    gboolean  use_accounting_labels;
    g_return_if_fail(to_info == NULL && from_info == NULL);

    use_accounting_labels = gnc_prefs_get_bool(GNC_PREFS_GROUP_GENERAL,
                                               GNC_PREF_ACCOUNTING_LABELS);

    ENTER(" ");
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-transfer.glade", "transfer_dialog");

    xferData->dialog = GTK_WIDGET(gtk_builder_get_object (builder, "transfer_dialog"));
    g_object_set_data_full (G_OBJECT (xferData->dialog), "builder", builder, g_object_unref);

    // Set the style context for this dialog so it can be easily manipulated with css
    gnc_widget_set_style_context (GTK_WIDGET(xferData->dialog), "GncTransferDialog");

    /* parent */
    if (parent != NULL)
        gtk_window_set_transient_for (GTK_WINDOW (xferData->dialog), GTK_WINDOW (parent));

    /* default to quickfilling off of the "From" account. */
    xferData->quickfill = XFER_DIALOG_FROM;

    xferData->transferinfo_label = GTK_WIDGET(gtk_builder_get_object (builder, "transferinfo-label"));

    xferData->fetch_button = GTK_WIDGET(gtk_builder_get_object (builder, "fetch"));
    gnc_xfer_dialog_set_fetch_sensitive (xferData->fetch_button);

    /* amount & date widgets */
    {
        GtkWidget *amount;
        GtkWidget *entry;
        GtkWidget *date;
        GtkWidget *hbox;

        amount = gnc_amount_edit_new();
        hbox = GTK_WIDGET(gtk_builder_get_object (builder, "amount_hbox"));
        gtk_box_pack_end(GTK_BOX(hbox), amount, TRUE, TRUE, 0);
        gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT (amount), TRUE);
        xferData->amount_edit = amount;

        entry = gnc_amount_edit_gtk_entry (GNC_AMOUNT_EDIT (amount));
        gtk_entry_set_activates_default (GTK_ENTRY(entry), TRUE);
        g_signal_connect (G_OBJECT (entry), "focus-out-event",
                          G_CALLBACK (gnc_xfer_amount_update_cb), xferData);

        date = gnc_date_edit_new(time (NULL), FALSE, FALSE);
        gnc_date_activates_default (GNC_DATE_EDIT(date), TRUE);
        hbox = GTK_WIDGET(gtk_builder_get_object (builder, "date_hbox"));

        gtk_box_pack_end(GTK_BOX(hbox), date, TRUE, TRUE, 0);
        xferData->date_entry = date;
        g_signal_connect (G_OBJECT (date), "date_changed",
                          G_CALLBACK (gnc_xfer_date_changed_cb), xferData);
    }

    {
        GtkWidget *entry;

        entry = GTK_WIDGET(gtk_builder_get_object (builder, "num_entry"));
        xferData->num_entry = entry;

        entry = GTK_WIDGET(gtk_builder_get_object (builder, "description_entry"));
        xferData->description_entry = entry;

        entry = GTK_WIDGET(gtk_builder_get_object (builder, "memo_entry"));
        xferData->memo_entry = entry;
    }

    /* from and to */
    {
        GtkWidget *label;
        gchar *text;

        to_info   = g_new0(AccountTreeFilterInfo, 1);
        from_info = g_new0(AccountTreeFilterInfo, 1);

        gnc_xfer_dialog_fill_tree_view (xferData, XFER_DIALOG_TO);
        gnc_xfer_dialog_fill_tree_view (xferData, XFER_DIALOG_FROM);

        /* Reverse from and to account trees when in "accountant" mode,
           see comment in function gnc_xfer_dialog_fill_tree_table */
        if (use_accounting_labels)
        {
            label = GTK_WIDGET(gtk_builder_get_object (builder, "right_trans_label"));
            xferData->from_transfer_label = label;

            label = GTK_WIDGET(gtk_builder_get_object (builder, "left_trans_label"));
            xferData->to_transfer_label = label;

            text = g_strconcat ("<b>", _("Credit Account"), "</b>", NULL);
            gtk_label_set_markup (GTK_LABEL (xferData->from_transfer_label), text);
            g_free (text);

            text = g_strconcat ("<b>", _("Debit Account"), "</b>", NULL);
            gtk_label_set_markup (GTK_LABEL (xferData->to_transfer_label), text);
            g_free (text);

            label = GTK_WIDGET(gtk_builder_get_object (builder, "right_currency_label"));
            xferData->from_currency_label = label;

            label = GTK_WIDGET(gtk_builder_get_object (builder, "left_currency_label"));
            xferData->to_currency_label = label;
        }
        else
        {
            label = GTK_WIDGET(gtk_builder_get_object (builder, "left_trans_label"));
            xferData->from_transfer_label = label;

            label = GTK_WIDGET(gtk_builder_get_object (builder, "right_trans_label"));
            xferData->to_transfer_label = label;

            text = g_strconcat ("<b>", _("Transfer From"), "</b>", NULL);
            gtk_label_set_markup (GTK_LABEL (xferData->from_transfer_label), text);
            g_free (text);

            text = g_strconcat ("<b>", _("Transfer To"), "</b>", NULL);
            gtk_label_set_markup (GTK_LABEL (xferData->to_transfer_label), text);

            label = GTK_WIDGET(gtk_builder_get_object (builder, "left_currency_label"));
            xferData->from_currency_label = label;

            label = GTK_WIDGET(gtk_builder_get_object (builder, "right_currency_label"));
            xferData->to_currency_label = label;
        }

        label = GTK_WIDGET(gtk_builder_get_object (builder, "conv_forward"));
        xferData->conv_forward = label;

        label = GTK_WIDGET(gtk_builder_get_object (builder, "conv_reverse"));
        xferData->conv_reverse = label;
    }

    /* optional intermediate currency account */
    {
        GtkWidget *table;
        GtkWidget *entry;
        GtkWidget *edit;
        GtkWidget *hbox;
        GtkWidget *button;

        table = GTK_WIDGET(gtk_builder_get_object (builder, "curr_transfer_table"));
        xferData->curr_xfer_table = table;

        edit = gnc_amount_edit_new();
        gnc_amount_edit_set_print_info(GNC_AMOUNT_EDIT(edit),
                                       gnc_default_print_info (FALSE));
        hbox = GTK_WIDGET(gtk_builder_get_object (builder, "price_hbox"));
        gtk_box_pack_start(GTK_BOX(hbox), edit, TRUE, TRUE, 0);
        xferData->price_edit = edit;
        entry = gnc_amount_edit_gtk_entry (GNC_AMOUNT_EDIT (edit));
        g_signal_connect (G_OBJECT (entry), "focus-out-event",
                          G_CALLBACK (gnc_xfer_price_update_cb), xferData);
        gtk_entry_set_activates_default(GTK_ENTRY (entry), TRUE);

        edit = gnc_amount_edit_new();
        hbox = GTK_WIDGET(gtk_builder_get_object (builder, "right_amount_hbox"));
        gtk_box_pack_start(GTK_BOX(hbox), edit, TRUE, TRUE, 0);
        xferData->to_amount_edit = edit;
        entry = gnc_amount_edit_gtk_entry (GNC_AMOUNT_EDIT (edit));
        g_signal_connect (G_OBJECT (entry), "focus-out-event",
                          G_CALLBACK (gnc_xfer_to_amount_update_cb), xferData);
        gtk_entry_set_activates_default(GTK_ENTRY(entry), TRUE);

        button = GTK_WIDGET(gtk_builder_get_object (builder, "price_radio"));
        xferData->price_radio = button;

        button = GTK_WIDGET(gtk_builder_get_object (builder, "amount_radio"));
        xferData->amount_radio = button;

        if (use_accounting_labels)
        {
            gtk_label_set_text(GTK_LABEL(gtk_bin_get_child (GTK_BIN(xferData->amount_radio))),
                               _("Debit Amount"));
        }
        else
        {
            gtk_label_set_text(GTK_LABEL(gtk_bin_get_child (GTK_BIN(xferData->amount_radio))),
                               _("To Amount"));
        }
    }

    gtk_builder_connect_signals(builder, xferData);
    gnc_restore_window_size (GNC_PREFS_GROUP,
                             GTK_WINDOW (xferData->dialog), GTK_WINDOW (parent));
    LEAVE(" ");
}

static void
close_handler (gpointer user_data)
{
    XferDialog *xferData = user_data;
    GtkWidget *dialog;

    ENTER(" ");
    dialog = GTK_WIDGET (xferData->dialog);

    gnc_save_window_size (GNC_PREFS_GROUP, GTK_WINDOW (dialog));
    gtk_widget_hide (dialog);
    gnc_xfer_dialog_close_cb(GTK_DIALOG(dialog), xferData);
    gtk_widget_destroy (dialog);
    g_free (to_info);
    to_info = NULL;
    g_free (from_info);
    from_info = NULL;
    LEAVE(" ");
}

/********************************************************************\
 * gnc_xfer_dialog                                                  *
 *   opens up a window to do an automatic transfer between accounts *
 *                                                                  *
 * Args:   parent  - the parent of the window to be created         *
 *         initial - the initial account in the from/to fields      *
 * Return: XferDialog structure                                     *
\********************************************************************/
XferDialog *
gnc_xfer_dialog (GtkWidget * parent, Account * initial)
{
    XferDialog *xferData;
    GNCAmountEdit *gae;
    GtkWidget *amount_entry;
    QofBook *book = NULL;

    xferData = g_new0 (XferDialog, 1);

    xferData->desc_start_selection = 0;
    xferData->desc_end_selection = 0;
    xferData->desc_selection_source_id = 0;
    xferData->quickfill = XFER_DIALOG_FROM;
    xferData->transaction_cb = NULL;

    if (initial)
    {
        book = gnc_account_get_book (initial);
    }
    else
    {
        book = gnc_get_current_book ();
    }

    xferData->book = book;
    xferData->pricedb = gnc_pricedb_get_db (book);

    gnc_xfer_dialog_create(parent, xferData);

    DEBUG("register component");
    gnc_register_gui_component (DIALOG_TRANSFER_CM_CLASS,
                                NULL, close_handler, xferData);

    gae = GNC_AMOUNT_EDIT(xferData->amount_edit);
    amount_entry = gnc_amount_edit_gtk_entry (gae);

    gtk_widget_grab_focus(amount_entry);

    gnc_xfer_dialog_select_from_account(xferData, initial);
    gnc_xfer_dialog_select_to_account(xferData, initial);

    gnc_xfer_dialog_curr_acct_activate(xferData);

    gtk_widget_show_all(xferData->dialog);

    gnc_window_adjust_for_screen(GTK_WINDOW(xferData->dialog));

    return xferData;
}

void
gnc_xfer_dialog_close( XferDialog *xferData )
{
    if ( xferData )
    {
        DEBUG("close component");
        gtk_dialog_response( GTK_DIALOG(xferData->dialog), GTK_RESPONSE_NONE );
    }
}

void
gnc_xfer_dialog_set_title( XferDialog *xferData, const gchar *title )
{
    if ( xferData && title )
    {
        gtk_window_set_title (GTK_WINDOW (xferData->dialog), title);
    }
}

void
gnc_xfer_dialog_set_information_label( XferDialog *xferData,
                                       const gchar *text )
{
    if (xferData && text)
    {
        gchar *markup_text = g_strdup_printf ("<b>%s</b>", text);
        gtk_label_set_markup (GTK_LABEL (xferData->transferinfo_label), markup_text);
        g_free (markup_text);
    }
}


static void
gnc_xfer_dialog_set_account_label( XferDialog *xferData,
                                   const gchar *text,
                                   XferDirection direction )
{
    if (xferData && text)
    {
        gchar *markup_text = g_strdup_printf ("<b>%s</b>", text);
        gtk_label_set_markup (GTK_LABEL ((direction == XFER_DIALOG_FROM ?
                                          xferData->from_transfer_label :
                                          xferData->to_transfer_label)),
                              markup_text);
        g_free (markup_text);
    }
}

void
gnc_xfer_dialog_set_from_account_label( XferDialog *xferData,
                                        const gchar *label )
{
    gnc_xfer_dialog_set_account_label (xferData, label, XFER_DIALOG_FROM);
}

void
gnc_xfer_dialog_set_to_account_label( XferDialog *xferData,
                                      const gchar *label )
{
    gnc_xfer_dialog_set_account_label (xferData, label, XFER_DIALOG_TO);
}

void
gnc_xfer_dialog_set_from_show_button_active( XferDialog *xferData,
                                             gboolean set_value )
{
    if ( xferData && xferData->from_show_button )
    {
        gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON(xferData->from_show_button),
                                      set_value );
    }
}

void
gnc_xfer_dialog_set_to_show_button_active( XferDialog *xferData,
                                           gboolean set_value )
{
    if ( xferData && xferData->to_show_button )
    {
        gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON(xferData->to_show_button),
                                      set_value );
    }
}

/* Add a button with a user-specified label and "clicked" callback */
void gnc_xfer_dialog_add_user_specified_button( XferDialog *xferData,
                                                const gchar *label,
                                                GCallback callback,
                                                gpointer user_data )
{
    if ( xferData && label && callback )
    {
        GtkBuilder *builder = g_object_get_data (G_OBJECT (xferData->dialog), "builder");
        GtkWidget *button   = gtk_button_new_with_label( label );
        GtkWidget *box      = GTK_WIDGET(gtk_builder_get_object (builder,
                                                                 "transfermain-vbox" ));
        gtk_box_pack_end( GTK_BOX(box), button, FALSE, FALSE, 0 );
        g_signal_connect (G_OBJECT (button), "clicked", G_CALLBACK (callback), user_data);
        gtk_widget_show( button );
    }
}

void gnc_xfer_dialog_toggle_currency_table( XferDialog *xferData,
                                            gboolean show_table )
{
    if (xferData && xferData->curr_xfer_table)
    {
        if (show_table)
            gtk_widget_show(xferData->curr_xfer_table);
        else
            gtk_widget_hide(xferData->curr_xfer_table);
    }
}


/* helper function */
static gboolean
find_xfer (gpointer find_data, gpointer user_data)
{
    return( find_data == user_data );
}

/* Run the dialog until the user has either successfully completed the
 * transaction (just clicking OK doesn't always count) or clicked Cancel.
 * Return TRUE if the transaction was a success, FALSE otherwise.
 */
gboolean gnc_xfer_dialog_run_until_done( XferDialog *xferData )
{
    GtkDialog *dialog;
    gint count, response;

    ENTER("xferData=%p", xferData);
    if ( xferData == NULL )
    {
        LEAVE("bad args");
        return( FALSE );
    }

    dialog = GTK_DIALOG (xferData->dialog);

    /*
     * We need to call the response_cb function by hand.  Calling it
     * automatically on a button click can destroy the window, and
     * that's bad mojo whole gtk_dialog_run is still in control.
     */
    count = g_signal_handlers_disconnect_by_func(dialog,
                                                 gnc_xfer_dialog_response_cb,
                                                 xferData);
    g_assert(count == 1);

    while ( TRUE )
    {
        DEBUG("calling gtk_dialog_run");
        response = gtk_dialog_run (dialog);
        DEBUG("gtk_dialog_run returned %d", response);
        gnc_xfer_dialog_response_cb (dialog, response, xferData);

        if ((response != GTK_RESPONSE_OK) && (response != GTK_RESPONSE_APPLY))
        {
            LEAVE("not ok");
            return FALSE;
        }

        /* See if the dialog is still there.  For various reasons, the
         * user could have hit OK but remained in the dialog.  We don't
         * want to return processing back to anyone else until we clear
         * off this dialog, so if the dialog is still there we'll just
         * run it again.
         */
        if ( !gnc_find_first_gui_component( DIALOG_TRANSFER_CM_CLASS,
                                            find_xfer, xferData ) )
        {
            /* no more dialog, and OK was clicked, so assume it's all good */
            LEAVE("ok");
            return TRUE;
        }

        /* else run the dialog again */
    }

    g_assert_not_reached();
    return FALSE; /* to satisfy static code analysis */
}


/* Indicate that the dialog should quickfill based on the "To" account,
 * rather than the default which is the "From" account.
 */

void
gnc_xfer_dialog_quickfill_to_account(XferDialog *xferData,
                                     gboolean qf_to_account )
{
    XferDirection old = xferData->quickfill;

    xferData->quickfill = qf_to_account ? XFER_DIALOG_TO : XFER_DIALOG_FROM;

    /* reload the quickfill if necessary */
    if ( old != xferData->quickfill )
        gnc_xfer_dialog_reload_quickfill( xferData );
}

static Account *
gnc_transfer_dialog_get_selected_account (XferDialog *dialog,
                                          XferDirection direction)
{
    GtkTreeView *tree_view;
    Account *account;

    switch (direction)
    {
        case XFER_DIALOG_FROM:
            tree_view = dialog->from_tree_view;
            break;
        case XFER_DIALOG_TO:
            tree_view = dialog->to_tree_view;
            break;
        default:
            g_assert_not_reached ();
            return NULL;
    }

    account = gnc_tree_view_account_get_selected_account  (GNC_TREE_VIEW_ACCOUNT (tree_view));
    return account;
}

static void
gnc_transfer_dialog_set_selected_account (XferDialog *dialog,
                                          Account *account,
                                          XferDirection direction)
{
    GtkTreeView *tree_view;
    GtkCheckButton *show_button;
    GNCAccountType type;

    if (account == NULL)
        return;

    switch (direction)
    {
        case XFER_DIALOG_FROM:
            tree_view = dialog->from_tree_view;
            show_button = GTK_CHECK_BUTTON (dialog->from_show_button);
            break;
        case XFER_DIALOG_TO:
            tree_view = dialog->to_tree_view;
            show_button = GTK_CHECK_BUTTON (dialog->to_show_button);
            break;
        default:
            g_assert_not_reached ();
            return;
    }

    type = xaccAccountGetType (account);
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (show_button),
                                  (type == ACCT_TYPE_EXPENSE) ||
                                  (type == ACCT_TYPE_INCOME));

    gnc_tree_view_account_set_selected_account (GNC_TREE_VIEW_ACCOUNT (tree_view),
                                                account);
}


void gnc_xfer_dialog_set_txn_cb(XferDialog *xferData,
                                gnc_xfer_dialog_cb handler,
                                gpointer user_data)
{
    g_assert(xferData);
    xferData->transaction_cb = handler;
    xferData->transaction_user_data = user_data;
}



gboolean gnc_xfer_dialog_run_exchange_dialog(
    XferDialog *xfer, gnc_numeric *exch_rate, gnc_numeric amount,
    Account *reg_acc, Transaction *txn, gnc_commodity *xfer_com,
    gboolean expanded)
{
    gboolean swap_amounts = FALSE;
    gnc_commodity *txn_cur = xaccTransGetCurrency(txn);
    gnc_commodity *reg_com = xaccAccountGetCommodity(reg_acc);

    g_return_val_if_fail(txn_cur && GNC_IS_COMMODITY (txn_cur), TRUE);
    g_return_val_if_fail(xfer_com && GNC_IS_COMMODITY (xfer_com), TRUE);

    if (xaccTransUseTradingAccounts (txn))
    {
        /* If we're using commodity trading accounts then "amount" is
           really the split's amount and it's in xfer_com commodity.
           We need an exchange rate that will convert this amount
           into a value in the transaction currency.  */
        if (gnc_commodity_equal(xfer_com, txn_cur))
        {
            /* Transaction is in the same currency as the split, exchange
               rate is 1. */
            *exch_rate = gnc_numeric_create(1, 1);
            return FALSE;
        }
        swap_amounts = expanded;
    }

    /* We know that "amount" is always in the reg_com currency.
     * Unfortunately it is possible that neither xfer_com or txn_cur are
     * the same as reg_com, in which case we need to convert to the txn
     * currency...  Or, if the register commodity is the xfer_com, then we
     * need to flip-flop the commodities and the exchange rates.
     */

    else if (gnc_commodity_equal(reg_com, txn_cur))
    {
        /* we're working in the txn currency.  Great.  Nothing to do! */
        swap_amounts = FALSE;

    }
    else if (gnc_commodity_equal(reg_com, xfer_com))
    {
        /* We're working in the xfer commodity.  Great.  Just swap the
           amounts. */
        swap_amounts = TRUE;

        /* XXX: Do we need to check for expanded v. non-expanded
           accounts here? */

    }
    else
    {
        /* UGGH -- we're not in either.  That means we need to convert
         * 'amount' from the register commodity to the txn currency.
         */
        gnc_numeric rate = xaccTransGetAccountConvRate(txn, reg_acc);

        /* XXX: should we tell the user we've done the conversion? */
        amount = gnc_numeric_div(amount, rate,
                                 gnc_commodity_get_fraction(txn_cur),
                                 GNC_HOW_DENOM_REDUCE);
    }

    /* enter the accounts */
    if (swap_amounts)
    {
        gnc_xfer_dialog_select_to_currency(xfer, txn_cur);
        gnc_xfer_dialog_select_from_currency(xfer, xfer_com);
        if (!gnc_numeric_zero_p(*exch_rate))
            *exch_rate = gnc_numeric_invert(*exch_rate);
        amount = gnc_numeric_neg(amount);
    }
    else
    {
        gnc_xfer_dialog_select_to_currency(xfer, xfer_com);
        gnc_xfer_dialog_select_from_currency(xfer, txn_cur);
        if (xaccTransUseTradingAccounts ( txn ))
            amount = gnc_numeric_neg(amount);
    }
    gnc_xfer_dialog_hide_to_account_tree(xfer);
    gnc_xfer_dialog_hide_from_account_tree(xfer);

    gnc_xfer_dialog_set_amount(xfer, amount);
    /* Now that from amount is set, set the to amount. */
    gnc_xfer_update_to_amount(xfer);

    /*
     * When we flip, we should tell the dialog so it can deal with the
     * pricedb properly.
     */

    /* Set the exchange rate */
    gnc_xfer_dialog_set_price_edit(xfer, *exch_rate);

    /* and run it... */
    if (gnc_xfer_dialog_run_until_done(xfer) == FALSE)
        return TRUE;
    /* If we inverted the rate for the dialog, invert it back. */
    if (swap_amounts)
        *exch_rate = gnc_numeric_invert(*exch_rate);

    return FALSE;
}
