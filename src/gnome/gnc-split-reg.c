/********************************************************************\
 * gnc-split-reg.c -- A widget for the common register look-n-feel. *
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

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <time.h>

#include "gnc-split-reg.h"

#include "Account.h"
#include "QueryNew.h"
#include "SX-book.h"
#include "dialog-account.h"
#include "dialog-sx-editor.h"
#include "dialog-sx-from-trans.h"
#include "gnc-component-manager.h"
#include "gnc-date-edit.h"
#include "gnc-engine.h"
#include "gnc-euro.h"
#include "gnc-gconf-utils.h"
#include "gnc-gui-query.h"
#include "gnc-ledger-display.h"
#include "gnc-pricedb.h"
#include "gnc-ui-util.h"
#include "gnc-ui.h"
#include "gnucash-sheet.h"
#include "table-allgui.h"

#include <libguile.h>
#include "dialog-utils.h"

// static QofLogModule log_module = GNC_MOD_SX;
static QofLogModule log_module = GNC_MOD_GUI;

/***** PROTOTYPES ***************************************************/
void gnc_split_reg_raise( GNCSplitReg *gsr );

static GtkWidget* add_summary_label( GtkWidget *summarybar,
                                     const char *label_str );

static void gnc_split_reg_determine_read_only( GNCSplitReg *gsr );

static GNCPlaceholderType gnc_split_reg_get_placeholder( GNCSplitReg *gsr );
static gncUIWidget gnc_split_reg_get_parent( GNCLedgerDisplay *ledger );

static void gsr_create_table( GNCSplitReg *gsr );
static void gsr_setup_table( GNCSplitReg *gsr );
static void gsr_setup_status_widgets( GNCSplitReg *gsr );

static void gsr_update_summary_label( GtkWidget *label,
                                      xaccGetBalanceFn getter,
                                      Account *leader,
                                      GNCPrintAmountInfo print_info,
                                      gnc_commodity *cmdty,
                                      gboolean reverse,
                                      gboolean euroFlag );

static void gsr_redraw_all_cb (GnucashRegister *g_reg, gpointer data);

static void gnc_split_reg_refresh_toolbar( GNCSplitReg *gsr );

static void gnc_split_reg_ld_destroy( GNCLedgerDisplay *ledger );

static Transaction* create_balancing_transaction(QofBook *book, Account *account,
        time_t statement_date, gnc_numeric balancing_amount);

void gsr_default_enter_handler    ( GNCSplitReg *w, gpointer ud );
void gsr_default_cancel_handler   ( GNCSplitReg *w, gpointer ud );
void gsr_default_delete_handler   ( GNCSplitReg *w, gpointer ud );
void gsr_default_reinit_handler   ( GNCSplitReg *w, gpointer ud );
void gsr_default_dup_handler      ( GNCSplitReg *w, gpointer ud );
void gsr_default_schedule_handler ( GNCSplitReg *w, gpointer ud );
void gsr_default_expand_handler   ( GNCSplitReg *w, gpointer ud );
void gsr_default_blank_handler    ( GNCSplitReg *w, gpointer ud );
void gsr_default_jump_handler     ( GNCSplitReg *w, gpointer ud );
void gsr_default_cut_handler      ( GNCSplitReg *w, gpointer ud );
void gsr_default_cut_txn_handler  ( GNCSplitReg *w, gpointer ud );
void gsr_default_copy_handler     ( GNCSplitReg *w, gpointer ud );
void gsr_default_copy_txn_handler ( GNCSplitReg *w, gpointer ud );
void gsr_default_paste_handler    ( GNCSplitReg *w, gpointer ud );
void gsr_default_paste_txn_handler( GNCSplitReg *w, gpointer ud );
void gsr_default_void_txn_handler ( GNCSplitReg *w, gpointer ud );
void gsr_default_unvoid_txn_handler  ( GNCSplitReg *w, gpointer ud );
void gsr_default_reverse_txn_handler ( GNCSplitReg *w, gpointer ud );

static void gsr_emit_simple_signal( GNCSplitReg *gsr, const char *sigName );
static void gsr_emit_help_changed( GnucashRegister *reg, gpointer user_data );
static void gsr_emit_include_date_signal( GNCSplitReg *gsr, time_t date );

void gnc_split_reg_cut_cb(GtkWidget *w, gpointer data);
void gnc_split_reg_copy_cb(GtkWidget *w, gpointer data);
void gnc_split_reg_paste_cb(GtkWidget *w, gpointer data);

void gnc_split_reg_cut_trans_cb(GtkWidget *w, gpointer data);
void gnc_split_reg_copy_trans_cb(GtkWidget *w, gpointer data);
void gnc_split_reg_paste_trans_cb(GtkWidget *w, gpointer data);
void gnc_split_reg_void_trans_cb(GtkWidget *w, gpointer data);
void gnc_split_reg_unvoid_trans_cb(GtkWidget *w, gpointer data);
void gnc_split_reg_reverse_trans_cb(GtkWidget *w, gpointer data);

void gnc_split_reg_record_cb (GnucashRegister *reg, gpointer data);
void gnc_split_reg_reinitialize_trans_cb(GtkWidget *w, gpointer data);
void gnc_split_reg_delete_trans_cb(GtkWidget *w, gpointer data);
void gnc_split_reg_duplicate_trans_cb(GtkWidget *w, gpointer data);
void gnc_split_reg_recur_cb(GtkWidget *w, gpointer data);
void gnc_split_reg_record_trans_cb(GtkWidget *w, gpointer data);
void gnc_split_reg_cancel_trans_cb(GtkWidget *w, gpointer data);

void gnc_split_reg_expand_trans_menu_cb(GtkWidget *widget, gpointer data);
void gnc_split_reg_expand_trans_toolbar_cb(GtkWidget *widget, gpointer data);
void gnc_split_reg_new_trans_cb(GtkWidget *widget, gpointer data);
void gnc_split_reg_jump_cb(GtkWidget *widget, gpointer data);

void gnc_split_reg_style_ledger_cb (GtkWidget *w, gpointer data);
void gnc_split_reg_style_auto_ledger_cb (GtkWidget *w, gpointer data);
void gnc_split_reg_style_journal_cb (GtkWidget *w, gpointer data);
void gnc_split_reg_double_line_cb (GtkWidget *w, gpointer data);

void gnc_split_reg_sort_standard_cb (GtkWidget *w, gpointer data);
void gnc_split_reg_sort_date_cb (GtkWidget *w, gpointer data);
void gnc_split_reg_sort_date_entered_cb (GtkWidget *w, gpointer data);
void gnc_split_reg_sort_date_reconciled_cb (GtkWidget *w, gpointer data);
void gnc_split_reg_sort_num_cb (GtkWidget *w, gpointer data);
void gnc_split_reg_sort_amount_cb (GtkWidget *w, gpointer data);
void gnc_split_reg_sort_memo_cb (GtkWidget *w, gpointer data);
void gnc_split_reg_sort_desc_cb (GtkWidget *w, gpointer data);
void gnc_split_reg_sort_action_cb (GtkWidget *w, gpointer data);
void gnc_split_reg_sort_notes_cb (GtkWidget *w, gpointer data);

void gnc_split_reg_destroy_cb(GtkWidget *widget, gpointer data);
void gnc_split_reg_size_allocate( GtkWidget *widget,
                                  GtkAllocation *allocation,
                                  gpointer user_data );


void gnc_split_reg_handle_exchange_cb (GtkWidget *w, gpointer data);

static void gnc_split_reg_class_init( GNCSplitRegClass *class );
static void gnc_split_reg_init( GNCSplitReg *gsr );
static void gnc_split_reg_init2( GNCSplitReg *gsr );

void gnc_split_register_size_allocate (GtkWidget *widget,
                                       GtkAllocation *allocation,
                                       gpointer user_data);

FROM_STRING_FUNC(SortType, ENUM_LIST_SORTTYPE)
AS_STRING_FUNC(SortType, ENUM_LIST_SORTTYPE)

GType
gnc_split_reg_get_type( void )
{
    static GType gnc_split_reg_type = 0;

    if (!gnc_split_reg_type)
    {
        GTypeInfo type_info =
        {
            sizeof(GNCSplitRegClass),      /* class_size */
            NULL,   			/* base_init */
            NULL,				/* base_finalize */
            (GClassInitFunc)gnc_split_reg_class_init,
            NULL,				/* class_finalize */
            NULL,				/* class_data */
            sizeof(GNCSplitReg),		/* */
            0,				/* n_preallocs */
            (GInstanceInitFunc)gnc_split_reg_init,
        };

        gnc_split_reg_type = g_type_register_static( GTK_TYPE_VBOX,
                             "GNCSplitReg",
                             &type_info, 0 );
    }

    return gnc_split_reg_type;
}

/* SIGNALS */
enum gnc_split_reg_signal_enum
{
    ENTER_ENT_SIGNAL,
    CANCEL_ENT_SIGNAL,
    DELETE_ENT_SIGNAL,
    REINIT_ENT_SIGNAL,
    DUP_ENT_SIGNAL,
    SCHEDULE_ENT_SIGNAL,
    EXPAND_ENT_SIGNAL,
    BLANK_SIGNAL,
    JUMP_SIGNAL,
    CUT_SIGNAL,
    CUT_TXN_SIGNAL,
    COPY_SIGNAL,
    COPY_TXN_SIGNAL,
    PASTE_SIGNAL,
    PASTE_TXN_SIGNAL,
    VOID_TXN_SIGNAL,
    UNVOID_TXN_SIGNAL,
    REVERSE_TXN_SIGNAL,
    HELP_CHANGED_SIGNAL,
    INCLUDE_DATE_SIGNAL,
    LAST_SIGNAL
};

static guint gnc_split_reg_signals[LAST_SIGNAL] = { 0 };

static void
gnc_split_reg_class_init( GNCSplitRegClass *class )
{
    int i;
    GtkObjectClass *object_class;
    static struct similar_signal_info
    {
        enum gnc_split_reg_signal_enum s;
        const char *signal_name;
        guint defaultOffset;
    } signals[] =
    {
        { ENTER_ENT_SIGNAL,    "enter_ent",    G_STRUCT_OFFSET( GNCSplitRegClass, enter_ent_cb ) },
        { CANCEL_ENT_SIGNAL,   "cancel_ent",   G_STRUCT_OFFSET( GNCSplitRegClass, cancel_ent_cb ) },
        { DELETE_ENT_SIGNAL,   "delete_ent",   G_STRUCT_OFFSET( GNCSplitRegClass, delete_ent_cb ) },
        { REINIT_ENT_SIGNAL,   "reinit_ent",   G_STRUCT_OFFSET( GNCSplitRegClass, reinit_ent_cb ) },
        { DUP_ENT_SIGNAL,      "dup_ent",      G_STRUCT_OFFSET( GNCSplitRegClass, dup_ent_cb ) },
        { SCHEDULE_ENT_SIGNAL, "schedule_ent", G_STRUCT_OFFSET( GNCSplitRegClass, schedule_ent_cb ) },
        { EXPAND_ENT_SIGNAL,   "expand_ent",   G_STRUCT_OFFSET( GNCSplitRegClass, expand_ent_cb ) },
        { BLANK_SIGNAL,        "blank",        G_STRUCT_OFFSET( GNCSplitRegClass, blank_cb ) },
        { JUMP_SIGNAL,         "jump",         G_STRUCT_OFFSET( GNCSplitRegClass, jump_cb ) },
        { CUT_SIGNAL,          "cut",          G_STRUCT_OFFSET( GNCSplitRegClass, cut_cb ) },
        { CUT_TXN_SIGNAL,      "cut_txn",      G_STRUCT_OFFSET( GNCSplitRegClass, cut_txn_cb ) },
        { COPY_SIGNAL,         "copy",         G_STRUCT_OFFSET( GNCSplitRegClass, copy_cb ) },
        { COPY_TXN_SIGNAL,     "copy_txn",     G_STRUCT_OFFSET( GNCSplitRegClass, copy_txn_cb ) },
        { PASTE_SIGNAL,        "paste",        G_STRUCT_OFFSET( GNCSplitRegClass, paste_cb ) },
        { PASTE_TXN_SIGNAL,    "paste_txn",    G_STRUCT_OFFSET( GNCSplitRegClass, paste_txn_cb ) },
        { VOID_TXN_SIGNAL,     "void_txn",     G_STRUCT_OFFSET( GNCSplitRegClass, void_txn_cb ) },
        { UNVOID_TXN_SIGNAL,   "unvoid_txn",   G_STRUCT_OFFSET( GNCSplitRegClass, unvoid_txn_cb ) },
        { REVERSE_TXN_SIGNAL,  "reverse_txn",  G_STRUCT_OFFSET( GNCSplitRegClass, reverse_txn_cb ) },
        { HELP_CHANGED_SIGNAL, "help-changed", G_STRUCT_OFFSET( GNCSplitRegClass, help_changed_cb ) },
        { INCLUDE_DATE_SIGNAL, "include-date", G_STRUCT_OFFSET( GNCSplitRegClass, include_date_cb ) },
        { LAST_SIGNAL, NULL, 0 }
    };

    object_class = (GtkObjectClass*) class;

    for ( i = 0; signals[i].s != INCLUDE_DATE_SIGNAL; i++ )
    {
        gnc_split_reg_signals[ signals[i].s ] =
            g_signal_new( signals[i].signal_name,
                          G_TYPE_FROM_CLASS(object_class),
                          G_SIGNAL_RUN_LAST,
                          signals[i].defaultOffset,
                          NULL, NULL,
                          g_cclosure_marshal_VOID__VOID,
                          G_TYPE_NONE, 0 );
    }
    /* Setup the non-default-marshalled signals; 'i' is still valid, here. */
    /* "include-date" */
    gnc_split_reg_signals[ INCLUDE_DATE_SIGNAL ] =
        g_signal_new( "include-date",
                      G_TYPE_FROM_CLASS(object_class),
                      G_SIGNAL_RUN_LAST,
                      signals[i++].defaultOffset,
                      NULL, NULL,
                      g_cclosure_marshal_VOID__INT, /* time_t == int */
                      G_TYPE_NONE, 1, G_TYPE_INT );

    g_assert( i == LAST_SIGNAL );

    /* Setup the default handlers. */
    class->enter_ent_cb    = gsr_default_enter_handler;
    class->cancel_ent_cb   = gsr_default_cancel_handler;
    class->delete_ent_cb   = gsr_default_delete_handler;
    class->reinit_ent_cb   = gsr_default_reinit_handler;
    class->dup_ent_cb      = gsr_default_dup_handler;
    class->schedule_ent_cb = gsr_default_schedule_handler;
    class->expand_ent_cb   = gsr_default_expand_handler;
    class->blank_cb        = gsr_default_blank_handler;
    class->jump_cb         = gsr_default_jump_handler;
    class->cut_cb          = gsr_default_cut_handler;
    class->cut_txn_cb      = gsr_default_cut_txn_handler;
    class->copy_cb         = gsr_default_copy_handler;
    class->copy_txn_cb     = gsr_default_copy_txn_handler;
    class->paste_cb        = gsr_default_paste_handler;
    class->paste_txn_cb    = gsr_default_paste_txn_handler;
    class->void_txn_cb     = gsr_default_void_txn_handler;
    class->unvoid_txn_cb   = gsr_default_unvoid_txn_handler;
    class->reverse_txn_cb  = gsr_default_reverse_txn_handler;

    class->help_changed_cb = NULL;
    class->include_date_cb = NULL;
}

GtkWidget*
gnc_split_reg_new( GNCLedgerDisplay *ld,
                   GtkWindow *parent,
                   gint numberOfLines,
                   gboolean read_only )
{
    GNCSplitReg *gsrToRet;

    ENTER("ld=%p, parent=%p, numberOfLines=%d, read_only=%s",
          ld, parent, numberOfLines, read_only ? "TRUE" : "FALSE");

    gsrToRet = g_object_new( gnc_split_reg_get_type(), NULL );

    gsrToRet->numRows        = numberOfLines;
    gsrToRet->read_only      = read_only;

    gsrToRet->ledger = ld;
    gsrToRet->window = GTK_WIDGET(parent);

    gnc_split_reg_init2( gsrToRet );

    LEAVE("%p", gsrToRet);
    return GTK_WIDGET( gsrToRet );
}

static void
gnc_split_reg_init( GNCSplitReg *gsr )
{
    gsr->sort_type = BY_STANDARD;
    gsr->width = -1;
    gsr->height = -1;
    gsr->numRows = gnc_gconf_get_float(GCONF_GENERAL_REGISTER,
                                       KEY_NUMBER_OF_ROWS, NULL);
    gsr->read_only = FALSE;

    g_signal_connect( gsr, "destroy",
                      G_CALLBACK (gnc_split_reg_destroy_cb), gsr );
}

static void
gnc_split_reg_init2( GNCSplitReg *gsr )
{
    if ( !gsr ) return;

    gnc_split_reg_determine_read_only( gsr );

    gsr_setup_status_widgets( gsr );
    /* ordering is important here... setup_status before create_table */
    gsr_create_table( gsr );
    gsr_setup_table( gsr );
}

static
void
gsr_setup_table( GNCSplitReg *gsr )
{
    SplitRegister *sr;

    ENTER("gsr=%p", gsr);

    sr = gnc_ledger_display_get_split_register( gsr->ledger );
    gnc_split_register_show_present_divider( sr, TRUE );
    /* events should be sufficient to redraw this */
    /* gnc_ledger_display_refresh( gsr->ledger ); */
    gnc_split_reg_refresh_toolbar( gsr );

    LEAVE(" ");
}

static
void
gsr_create_table( GNCSplitReg *gsr )
{
    GtkWidget *register_widget;
    SplitRegister *sr;

    ENTER("gsr=%p", gsr);

    gnc_ledger_display_set_user_data( gsr->ledger, (gpointer)gsr );
    gnc_ledger_display_set_handlers( gsr->ledger,
                                     gnc_split_reg_ld_destroy,
                                     gnc_split_reg_get_parent );

    /* FIXME: We'd really rather pass this down... */
    sr = gnc_ledger_display_get_split_register( gsr->ledger );
    gnucash_register_set_initial_rows( gsr->numRows );
    register_widget = gnucash_register_new( sr->table );
    gsr->reg = GNUCASH_REGISTER( register_widget );
    gnc_table_init_gui( GTK_WIDGET(gsr->reg), sr );

    gtk_box_pack_start (GTK_BOX (gsr), GTK_WIDGET(gsr->reg), TRUE, TRUE, 0);
    GNUCASH_SHEET(gsr->reg->sheet)->window = gsr->window;
    gtk_widget_show ( GTK_WIDGET(gsr->reg) );
    g_signal_connect (gsr->reg, "activate_cursor",
                      G_CALLBACK(gnc_split_reg_record_cb), gsr);
    g_signal_connect (gsr->reg, "redraw_all",
                      G_CALLBACK(gsr_redraw_all_cb), gsr);
    g_signal_connect (gsr->reg, "redraw_help",
                      G_CALLBACK(gsr_emit_help_changed), gsr);

    LEAVE(" ");
}

static
void
gsr_setup_status_widgets( GNCSplitReg *gsr )
{
    SplitRegister *sr;
    gboolean use_double_line;

    sr = gnc_ledger_display_get_split_register( gsr->ledger );
    use_double_line = gnc_ledger_display_default_double_line( gsr->ledger );

    /* be sure to initialize the gui elements associated with the cursor */
    gnc_split_register_config( sr, sr->type, sr->style, use_double_line );
}

void
gnc_split_reg_destroy_cb(GtkWidget *widget, gpointer data)
{
}

/**
 * Raise an existing register window to the front.
 **/
void
gnc_split_reg_raise( GNCSplitReg *gsr )
{
    if (gsr == NULL)
        return;

    if (gsr->window == NULL)
        return;

    gtk_window_present( GTK_WINDOW(gsr->window) );
}


/**
 * Duplicate-code reduction function; retreives, formats and updates the
 * GtkLabel with the given amount.
 **/
static
void
gsr_update_summary_label( GtkWidget *label,
                          xaccGetBalanceFn getter,
                          Account *leader,
                          GNCPrintAmountInfo print_info,
                          gnc_commodity *cmdty,
                          gboolean reverse,
                          gboolean euroFlag )
{
    gnc_numeric amount;
    char string[256];

    if ( label == NULL )
        return;

    amount = (*getter)( leader );

    if ( reverse )
    {
        amount = gnc_numeric_neg( amount );
    }

    xaccSPrintAmount( string, amount, print_info );

    if ( euroFlag )
    {
        strcat( string, " / " );
        xaccSPrintAmount( string + strlen( string ),
                          gnc_convert_to_euro( cmdty, amount ),
                          gnc_commodity_print_info( gnc_get_euro(), TRUE ) );
    }

    gnc_set_label_color( label, amount );
    gtk_label_set_text( GTK_LABEL(label), string );
}

static GNCPrice *
account_latest_price (Account *account)
{
    QofBook *book;
    GNCPriceDB *pdb;
    gnc_commodity *commodity;
    gnc_commodity *currency;

    if (!account) return NULL;
    commodity = xaccAccountGetCommodity (account);
    currency = gnc_default_currency ();

    book = gnc_account_get_book (account);
    pdb = gnc_book_get_pricedb (book);

    return gnc_pricedb_lookup_latest (pdb, commodity, currency);
}

static GNCPrice *
account_latest_price_any_currency (Account *account)
{
    QofBook *book;
    GNCPriceDB *pdb;
    gnc_commodity *commodity;
    GList *price_list;
    GNCPrice *result;

    if (!account) return NULL;
    commodity = xaccAccountGetCommodity (account);

    book = gnc_account_get_book (account);
    pdb = gnc_book_get_pricedb (book);

    price_list = gnc_pricedb_lookup_latest_any_currency (pdb, commodity);
    if (!price_list) return NULL;

    result = gnc_price_clone((GNCPrice *)(price_list->data), book);

    gnc_price_list_destroy(price_list);

    return result;
}

static
void
gsr_redraw_all_cb (GnucashRegister *g_reg, gpointer data)
{
    GNCSplitReg *gsr = data;
    gnc_commodity * commodity;
    GNCPrintAmountInfo print_info;
    gnc_numeric amount;
    char string[256];
    Account *leader;
    gboolean reverse;
    gboolean euro;

    if ( gsr->summarybar == NULL )
        return;

    leader = gnc_ledger_display_leader( gsr->ledger );

    commodity = xaccAccountGetCommodity( leader );

    /* no EURO converson, if account is already EURO or no EURO currency */
    if (commodity != NULL)
        euro = (gnc_is_euro_currency( commodity ) &&
                (strncasecmp(gnc_commodity_get_mnemonic(commodity), "EUR", 3)));
    else
        euro = FALSE;

    print_info = gnc_account_print_info( leader, TRUE );
    reverse = gnc_reverse_balance( leader );

    gsr_update_summary_label( gsr->balance_label,
                              xaccAccountGetPresentBalance,
                              leader, print_info, commodity, reverse, euro );
    gsr_update_summary_label( gsr->cleared_label,
                              xaccAccountGetClearedBalance,
                              leader, print_info, commodity, reverse, euro );
    gsr_update_summary_label( gsr->reconciled_label,
                              xaccAccountGetReconciledBalance,
                              leader, print_info, commodity, reverse, euro );
    gsr_update_summary_label( gsr->future_label,
                              xaccAccountGetBalance,
                              leader, print_info, commodity, reverse, euro );
    gsr_update_summary_label( gsr->projectedminimum_label,
                              xaccAccountGetProjectedMinimumBalance,
                              leader, print_info, commodity, reverse, euro );

    /* Print the summary share amount */
    if (gsr->shares_label != NULL)
    {
        print_info = gnc_account_print_info( leader, TRUE );

        amount = xaccAccountGetBalance( leader );
        if (reverse)
            amount = gnc_numeric_neg( amount );

        xaccSPrintAmount( string, amount, print_info );

        gnc_set_label_color( gsr->shares_label, amount );
        gtk_label_set_text( GTK_LABEL(gsr->shares_label), string );
    }

    /* Print the summary share value */
    if (gsr->value_label != NULL)
    {
        GNCPrice *price;

        amount = xaccAccountGetBalance (leader);
        if (reverse) amount = gnc_numeric_neg (amount);

        price = account_latest_price (leader);
        if (!price)
        {
            /* If the balance is zero, then print zero. */
            if (gnc_numeric_equal(amount, gnc_numeric_zero()))
            {
                gnc_commodity *currency = gnc_default_currency ();
                print_info = gnc_commodity_print_info (currency, TRUE);
                amount = gnc_numeric_zero ();

                xaccSPrintAmount (string, amount, print_info);

                gnc_set_label_color (gsr->value_label, amount);
                gtk_label_set_text (GTK_LABEL (gsr->value_label), string);
            }
            else
            {
                /* else try to do a double-price-conversion :-( */
                price = account_latest_price_any_currency (leader);
                if (!price)
                {
                    gnc_set_label_color (gsr->value_label, gnc_numeric_zero ());
                    gtk_label_set_text (GTK_LABEL (gsr->value_label),
                                        _("<No information>"));
                }
                else
                {
                    gnc_commodity *currency = gnc_price_get_currency (price);
                    gnc_commodity *default_currency = gnc_default_currency ();
                    gnc_numeric currency_amount;
                    gnc_numeric default_currency_amount;

                    print_info = gnc_commodity_print_info (currency, TRUE);

                    currency_amount =
                        xaccAccountConvertBalanceToCurrency(leader, amount,
                                                            commodity, currency);
                    xaccSPrintAmount (string, currency_amount, print_info);

                    default_currency_amount =
                        xaccAccountConvertBalanceToCurrency(leader, amount,
                                                            commodity,
                                                            default_currency);
                    if (!gnc_numeric_zero_p(default_currency_amount))
                    {
                        strcat( string, " / " );
                        print_info = gnc_commodity_print_info (default_currency, TRUE);
                        xaccSPrintAmount( string + strlen( string ), default_currency_amount,
                                          print_info);
                    }

                    gnc_set_label_color (gsr->value_label, amount);
                    gtk_label_set_text (GTK_LABEL (gsr->value_label), string);

                    gnc_price_unref (price);
                }
            }
        }
        else
        {
            gnc_commodity *currency = gnc_price_get_currency (price);

            print_info = gnc_commodity_print_info (currency, TRUE);

            amount = gnc_numeric_mul (amount, gnc_price_get_value (price),
                                      gnc_commodity_get_fraction (currency),
                                      GNC_RND_ROUND);

            xaccSPrintAmount (string, amount, print_info);

            gnc_set_label_color (gsr->value_label, amount);
            gtk_label_set_text (GTK_LABEL (gsr->value_label), string);

            gnc_price_unref (price);
        }
    }
}

static void
gnc_split_reg_refresh_toolbar( GNCSplitReg *gsr )
{
    GtkToolbarStyle tbstyle;

    if ((gsr == NULL) || (gsr->toolbar == NULL))
        return;

    tbstyle = gnc_get_toolbar_style ();
    gtk_toolbar_set_style( GTK_TOOLBAR(gsr->toolbar), tbstyle );
}

static void
gnc_split_reg_ld_destroy( GNCLedgerDisplay *ledger )
{
    GNCSplitReg *gsr = gnc_ledger_display_get_user_data( ledger );

    if (gsr)
    {
        SplitRegister *reg;

        reg = gnc_ledger_display_get_split_register (ledger);

        if (reg && reg->table)
            gnc_table_save_state (reg->table);

        /*
         * Don't destroy the window here any more.  The register no longer
         * owns it.
         */
    }
    gnc_ledger_display_set_user_data (ledger, NULL);
}

void
gsr_default_cut_handler( GNCSplitReg *gsr, gpointer data )
{
    gnucash_register_cut_clipboard( gsr->reg );
}

/**
 * Cut the selection to the clipboard.  This refers to the Split.
 **/
void
gnc_split_reg_cut_cb (GtkWidget *w, gpointer data)
{
    GNCSplitReg *gsr = data;
    gsr_emit_simple_signal( gsr, "cut" );
}

void
gsr_default_copy_handler( GNCSplitReg *gsr, gpointer data )
{
    gnucash_register_copy_clipboard( gsr->reg );
}

/**
 * Copy the selection to the clipboard.  This refers to the Split.
 **/
void
gnc_split_reg_copy_cb (GtkWidget *w, gpointer data)
{
    GNCSplitReg *gsr = data;
    gsr_emit_simple_signal( gsr, "copy" );
}

void
gsr_default_paste_handler( GNCSplitReg *gsr, gpointer data )
{
    gnucash_register_paste_clipboard( gsr->reg );
}

/**
 * Paste the clipboard to the selection.  This refers to the Split.
 **/
void
gnc_split_reg_paste_cb (GtkWidget *w, gpointer data)
{
    GNCSplitReg *gsr = data;
    gsr_emit_simple_signal( gsr, "paste" );
}

void
gsr_default_cut_txn_handler( GNCSplitReg *gsr, gpointer data )
{
    gnc_split_register_cut_current
    (gnc_ledger_display_get_split_register( gsr->ledger ));
}

/**
 * Cut the current transaction  to the clipboard.
 **/
void
gnc_split_reg_cut_trans_cb (GtkWidget *w, gpointer data)
{
    GNCSplitReg *gsr = data;
    gsr_emit_simple_signal( gsr, "cut_txn" );
}

void
gsr_default_copy_txn_handler( GNCSplitReg *gsr, gpointer data )
{
    gnc_split_register_copy_current
    (gnc_ledger_display_get_split_register( gsr->ledger ));
}

/**
 * Copy the current transaction to the clipboard.
 **/
void
gnc_split_reg_copy_trans_cb(GtkWidget *w, gpointer data)
{
    GNCSplitReg *gsr = data;
    gsr_emit_simple_signal( gsr, "copy_txn" );
}

void
gsr_default_paste_txn_handler( GNCSplitReg *gsr, gpointer data )
{
    gnc_split_register_paste_current
    (gnc_ledger_display_get_split_register( gsr->ledger ));
}

/**
 * Paste the transaction clipboard to the selection.
 **/
void
gnc_split_reg_paste_trans_cb (GtkWidget *w, gpointer data)
{
    GNCSplitReg *gsr = data;
    gsr_emit_simple_signal( gsr, "paste_txn" );
}

/********************************************************************\
 * gnc_split_reg_void_trans_cb                                      *
 *                                                                  *
 * Args:   widget - the widget that called us                       *
 *         data   - the data struct for this register               *
 * Return: none                                                     *
\********************************************************************/
void
gsr_default_void_txn_handler (GNCSplitReg *gsr, gpointer data)
{
    // Override this function.
}

void
gnc_split_reg_void_trans_cb (GtkWidget *w, gpointer data)
{
    GNCSplitReg *gsr = data;
    gsr_emit_simple_signal( gsr, "void_txn" );
}

/********************************************************************\
 * gnc_split_reg_unvoid_trans_cb                                      *
 *                                                                  *
 * Args:   widget - the widget that called us                       *
 *         data   - the data struct for this register               *
 * Return: none                                                     *
\********************************************************************/
void
gsr_default_unvoid_txn_handler (GNCSplitReg *gsr, gpointer data)
{
    // Override this function.
}

void
gnc_split_reg_unvoid_trans_cb (GtkWidget *w, gpointer data)
{
    GNCSplitReg *gsr = data;
    gsr_emit_simple_signal( gsr, "unvoid_txn" );
}

/********************************************************************\
 * gnc_split_reg_reverse_trans_cb                                   *
 *                                                                  *
 * Args:   widget - the widget that called us                       *
 *         data   - the data struct for this register               *
 * Return: none                                                     *
\********************************************************************/
void
gsr_default_reverse_txn_handler (GNCSplitReg *gsr, gpointer data)
{
    SplitRegister *reg;
    Transaction *trans, *new_trans;

    reg = gnc_ledger_display_get_split_register( gsr->ledger );
    trans = gnc_split_register_get_current_trans (reg);
    if (trans == NULL)
        return;

    if (xaccTransGetReversedBy(trans))
    {
        gnc_error_dialog(gsr->window, "%s",
                         _("A reversing entry has already been created for this transaction."));
        return;
    }

    new_trans = xaccTransReverse(trans);

    /* Clear transaction level info */
    xaccTransSetDatePostedSecs(new_trans, time(NULL));
    xaccTransSetDateEnteredSecs(new_trans, time(NULL));

    /* Now jump to new trans */
    gnc_split_reg_jump_to_split(gsr, xaccTransGetSplit(new_trans, 0));
}

void
gnc_split_reg_reverse_trans_cb (GtkWidget *w, gpointer data)
{
    GNCSplitReg *gsr = data;
    gsr_emit_simple_signal( gsr, "reverse_txn" );
}


static gboolean
xaccTransWarnReadOnly (const Transaction *trans)
{
    GtkWidget *dialog;
    const gchar *reason;
    const gchar *title = _("Cannot modify or delete this transaction.");
    const gchar *message =
        _("This transaction is marked read-only with the comment: '%s'");

    if (!trans) return FALSE;

    reason = xaccTransGetReadOnly (trans);
    if (reason)
    {
        dialog = gtk_message_dialog_new(NULL,
                                        0,
                                        GTK_MESSAGE_ERROR,
                                        GTK_BUTTONS_OK,
                                        "%s", title);
        gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(dialog),
                message, reason);
        gtk_dialog_run(GTK_DIALOG(dialog));
        gtk_widget_destroy(dialog);
        return TRUE;
    }
    return FALSE;
}


void
gsr_default_reinit_handler( GNCSplitReg *gsr, gpointer data )
{
    VirtualCellLocation vcell_loc;
    SplitRegister *reg;
    Transaction *trans;
    Split *split;
    GtkWidget *dialog;
    gint response;
    const gchar *warning;

    const char *title = _("Remove the splits from this transaction?");
    const char *recn_warn = _("This transaction contains reconciled splits. "
                              "Modifying it is not a good idea because that will "
                              "cause your reconciled balance to be off.");

    reg = gnc_ledger_display_get_split_register( gsr->ledger );

    trans = gnc_split_register_get_current_trans (reg);
    if (xaccTransWarnReadOnly(trans))
        return;
    dialog = gtk_message_dialog_new(GTK_WINDOW(gsr->window),
                                    GTK_DIALOG_DESTROY_WITH_PARENT,
                                    GTK_MESSAGE_WARNING,
                                    GTK_BUTTONS_NONE,
                                    "%s", title);
    if (xaccTransHasReconciledSplits (trans))
    {
        gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(dialog),
                "%s", recn_warn);
        warning = "register_remove_all_splits2";
    }
    else
    {
        warning = "register_remove_all_splits";
    }

    gtk_dialog_add_button(GTK_DIALOG(dialog),
                          GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL);
    gnc_gtk_dialog_add_button(dialog, _("_Remove Splits"),
                              GTK_STOCK_DELETE, GTK_RESPONSE_ACCEPT);
    response = gnc_dialog_run(GTK_DIALOG(dialog), warning);
    gtk_widget_destroy (dialog);
    if (response != GTK_RESPONSE_ACCEPT)
        return;

    /*
     * Find the "transaction" split for the current transaction. This is
     * the split that appears at the top of the transaction in the
     * register.
     */
    split = gnc_split_register_get_current_split (reg);
    if (!gnc_split_register_get_split_virt_loc(reg, split, &vcell_loc))
        return;
    split = gnc_split_register_get_current_trans_split (reg, &vcell_loc);
    gnc_split_register_empty_current_trans_except_split (reg, split);
}

/**
 * "Reinitializes" the current transaction.
 **/
void
gnc_split_reg_reinitialize_trans_cb(GtkWidget *widget, gpointer data)
{
    GNCSplitReg *gsr = data;
    gsr_emit_simple_signal( gsr, "reinit_ent" );
}

void
gsr_default_delete_handler( GNCSplitReg *gsr, gpointer data )
{
    SplitRegisterStyle style;
    CursorClass cursor_class;
    SplitRegister *reg;
    Transaction *trans;
    Split *split;
    GtkWidget *dialog;
    gint response;
    const gchar *warning;

    reg = gnc_ledger_display_get_split_register( gsr->ledger );

    /* get the current split based on cursor position */
    split = gnc_split_register_get_current_split(reg);
    if (split == NULL)
    {
        gnc_split_register_cancel_cursor_split_changes (reg);
        return;
    }

    trans = xaccSplitGetParent(split);
    style = reg->style;
    cursor_class = gnc_split_register_get_current_cursor_class (reg);

    /* Deleting the blank split just cancels */
    {
        Split *blank_split = gnc_split_register_get_blank_split (reg);

        if (split == blank_split)
        {
            gnc_split_register_cancel_cursor_trans_changes (reg);
            return;
        }
    }

    if (cursor_class == CURSOR_CLASS_NONE)
        return;

    if (xaccTransWarnReadOnly(trans))
        return;

    /* On a split cursor, just delete the one split. */
    if (cursor_class == CURSOR_CLASS_SPLIT)
    {
        const char *format = _("Delete the split '%s' from the transaction '%s'?");
        const char *recn_warn = _("You would be deleting a reconciled split! "
                                  "This is not a good idea as it will cause your "
                                  "reconciled balance to be off.");
        const char *anchor_error = _("You cannot delete this split.");
        const char *anchor_split = _("This is the split anchoring this transaction "
                                     "to the register. You may not delete it from "
                                     "this register window.  You may delete the "
                                     "entire transaction from this window, or you "
                                     "may navigate to a register that shows "
                                     "another side of this same transaction and "
                                     "delete the split from that register.");
        char *buf = NULL;
        const char *memo;
        const char *desc;
        char recn;

        if (split == gnc_split_register_get_current_trans_split (reg, NULL))
        {
            dialog = gtk_message_dialog_new(GTK_WINDOW(gsr->window),
                                            GTK_DIALOG_MODAL
                                            | GTK_DIALOG_DESTROY_WITH_PARENT,
                                            GTK_MESSAGE_ERROR,
                                            GTK_BUTTONS_OK,
                                            "%s", anchor_error);
            gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(dialog),
                    "%s", anchor_split);
            gtk_dialog_run(GTK_DIALOG(dialog));
            gtk_widget_destroy (dialog);
            return;
        }

        memo = xaccSplitGetMemo (split);
        memo = (memo && *memo) ? memo : _("(no memo)");

        desc = xaccTransGetDescription (trans);
        desc = (desc && *desc) ? desc : _("(no description)");

        /* ask for user confirmation before performing permanent damage */
        buf = g_strdup_printf (format, memo, desc);
        dialog = gtk_message_dialog_new(GTK_WINDOW(gsr->window),
                                        GTK_DIALOG_MODAL
                                        | GTK_DIALOG_DESTROY_WITH_PARENT,
                                        GTK_MESSAGE_QUESTION,
                                        GTK_BUTTONS_NONE,
                                        "%s", buf);
        g_free(buf);
        recn = xaccSplitGetReconcile (split);
        if (recn == YREC || recn == FREC)
        {
            gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(dialog),
                    "%s", recn_warn);
            warning = "register_delete_split2";
        }
        else
        {
            warning = "register_delete_split";
        }

        gtk_dialog_add_button(GTK_DIALOG(dialog),
                              GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL);
        gnc_gtk_dialog_add_button(dialog, _("_Delete Split"),
                                  GTK_STOCK_DELETE, GTK_RESPONSE_ACCEPT);
        response = gnc_dialog_run(GTK_DIALOG(dialog), warning);
        gtk_widget_destroy (dialog);
        if (response != GTK_RESPONSE_ACCEPT)
            return;

        gnc_split_register_delete_current_split (reg);
        return;
    }

    g_return_if_fail(cursor_class == CURSOR_CLASS_TRANS);

    /* On a transaction cursor with 2 or fewer splits in single or double
     * mode, we just delete the whole transaction, kerblooie */
    {
        const char *title = _("Delete the current transaction?");
        const char *recn_warn = _("You would be deleting a transaction "
                                  "with reconciled splits! "
                                  "This is not a good idea as it will cause your "
                                  "reconciled balance to be off.");

        dialog = gtk_message_dialog_new(GTK_WINDOW(gsr->window),
                                        GTK_DIALOG_MODAL
                                        | GTK_DIALOG_DESTROY_WITH_PARENT,
                                        GTK_MESSAGE_WARNING,
                                        GTK_BUTTONS_NONE,
                                        "%s", title);
        if (xaccTransHasReconciledSplits (trans))
        {
            gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(dialog),
                    "%s", recn_warn);
            warning = "register_delete_trans2";
        }
        else
        {
            warning = "register_delete_trans";
        }
        gtk_dialog_add_button(GTK_DIALOG(dialog),
                              GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL);
        gnc_gtk_dialog_add_button(dialog, _("_Delete Transaction"),
                                  GTK_STOCK_DELETE, GTK_RESPONSE_ACCEPT);
        response =  gnc_dialog_run(GTK_DIALOG(dialog), warning);
        gtk_widget_destroy (dialog);
        if (response != GTK_RESPONSE_ACCEPT)
            return;

        gnc_split_register_delete_current_trans (reg);
        return;
    }
}

/**
 * Deletes the current transaction.
 **/
void
gnc_split_reg_delete_trans_cb(GtkWidget *widget, gpointer data)
{
    GNCSplitReg *gsr = data;
    gsr_emit_simple_signal( gsr, "delete_ent" );
}

void
gsr_default_dup_handler( GNCSplitReg *gsr, gpointer data )
{
    gnc_split_register_duplicate_current
    (gnc_ledger_display_get_split_register( gsr->ledger ));
}

/**
 * Duplicates the current transaction in the register.
 **/
void
gnc_split_reg_duplicate_trans_cb(GtkWidget *w, gpointer data)
{
    GNCSplitReg *gsr = data;
    gsr_emit_simple_signal( gsr, "dup_ent" );
}

/**
 * Schedules the current transaction for recurring-entry.
 * If the selected transaction was created from a scheduled transaction,
 * opens the editor for that Scheduled Transaction.
 **/
void
gsr_default_schedule_handler( GNCSplitReg *gsr, gpointer data )
{
    SplitRegister *reg = gnc_ledger_display_get_split_register( gsr->ledger );
    Transaction *pending_trans = gnc_split_register_get_current_trans (reg);

    /* If the transaction has a sched-xact KVP frame, then go to the editor
     * for the existing SX; otherwise, do the sx-from-trans dialog. */
    {
        kvp_frame *txn_frame;
        kvp_value *kvp_val;
        /* set a kvp-frame element in the transaction indicating and
         * pointing-to the SX this was created from. */
        txn_frame = xaccTransGetSlots( pending_trans );
        if ( txn_frame != NULL )
        {
            kvp_val = kvp_frame_get_slot( txn_frame, "from-sched-xaction" );
            if ( kvp_val )
            {
                GncGUID *fromSXId = kvp_value_get_guid( kvp_val );
                SchedXaction *theSX = NULL;
                GList *sxElts;

                /* Get the correct SX */
                for ( sxElts = gnc_book_get_schedxactions(gnc_get_current_book())->sx_list;
                        (!theSX) && sxElts;
                        sxElts = sxElts->next )
                {
                    SchedXaction *sx = (SchedXaction*)sxElts->data;
                    theSX =
                        ( ( guid_equal( xaccSchedXactionGetGUID( sx ), fromSXId ) )
                          ? sx : NULL );
                }

                if ( theSX )
                {
                    gnc_ui_scheduled_xaction_editor_dialog_create(theSX, FALSE);
                    return;
                }
            }
        }
    }

    gnc_sx_create_from_trans(pending_trans);
}

void
gnc_split_reg_recur_cb(GtkWidget *w, gpointer data)
{
    GNCSplitReg *gsr = data;
    gsr_emit_simple_signal( gsr, "schedule_ent" );
}

/**
 * Records into the books the currently-selected transaction.
 **/
void
gnc_split_reg_record_trans_cb (GtkWidget *w, gpointer data)
{
    GNCSplitReg *gsr = data;
    gsr_emit_simple_signal( gsr, "enter_ent" );
}

void
gsr_default_cancel_handler( GNCSplitReg *gsr, gpointer data )
{
    gnc_split_register_cancel_cursor_trans_changes
    (gnc_ledger_display_get_split_register( gsr->ledger ));
}

/**
 * Cancels the edits of the currently-selected transaction.
 **/
void
gnc_split_reg_cancel_trans_cb(GtkWidget *w, gpointer data)
{
    GNCSplitReg *gsr = data;
    gsr_emit_simple_signal( gsr, "cancel_ent" );
}

void
gsr_default_expand_handler( GNCSplitReg *gsr, gpointer data )
{
    gint activeCount;
    gboolean expand;
    SplitRegister *reg;

    if (!gsr)
        return;

    reg = gnc_ledger_display_get_split_register (gsr->ledger);

    /* These should all be in agreement. */
    activeCount =
        ( ( GTK_CHECK_MENU_ITEM(gsr->split_menu_check)->active ? 1 : -1 )
          + ( GTK_CHECK_MENU_ITEM(gsr->split_popup_check)->active ? 1 : -1 )
          + ( gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON(gsr->split_button) )
              ? 1 : -1 ) );

    /* If activeCount > 0, then there's more active than inactive; otherwise,
     * more inactive than active.  Both determine which state the user is
     * attempting to get to. */
    expand = ( activeCount < 0 );

    /* The ledger's invocation of 'redraw_all' will force the agreement in the
     * other split state widgets, so we neglect doing it here.  */
    gnc_split_register_expand_current_trans (reg, expand);
}

void
gnc_split_reg_expand_trans_menu_cb (GtkWidget *widget, gpointer data)
{
    GNCSplitReg *gsr = data;
    gsr_emit_simple_signal( gsr, "expand_ent" );
}

void
gnc_split_reg_expand_trans_toolbar_cb (GtkWidget *widget, gpointer data)
{
    GNCSplitReg *gsr = data;
    gsr_emit_simple_signal( gsr, "expand_ent" );
}

/**
 * move the cursor to the split, if present in register
**/
void
gnc_split_reg_jump_to_split(GNCSplitReg *gsr, Split *split)
{
    Transaction *trans;
    VirtualCellLocation vcell_loc;
    SplitRegister *reg;

    if (!gsr) return;

    trans = xaccSplitGetParent(split);

    gsr_emit_include_date_signal( gsr, xaccTransGetDate(trans) );

    reg = gnc_ledger_display_get_split_register( gsr->ledger );

    if (gnc_split_register_get_split_virt_loc(reg, split, &vcell_loc))
        gnucash_register_goto_virt_cell( gsr->reg, vcell_loc );

    gnc_ledger_display_refresh( gsr->ledger );
}


/**
 * Move the cursor to the split in the non-blank amount column.
 **/
void
gnc_split_reg_jump_to_split_amount(GNCSplitReg *gsr, Split *split)
{
    VirtualLocation virt_loc;
    SplitRegister *reg;
    Transaction *trans;

    if (!gsr) return;

    trans = xaccSplitGetParent(split);
    gsr_emit_include_date_signal( gsr, xaccTransGetDate(trans) );

    reg = gnc_ledger_display_get_split_register (gsr->ledger);

    if (gnc_split_register_get_split_amount_virt_loc (reg, split, &virt_loc))
        gnucash_register_goto_virt_loc (gsr->reg, virt_loc);

    gnc_ledger_display_refresh (gsr->ledger);
}

void
gnc_split_reg_jump_to_blank (GNCSplitReg *gsr)
{
    SplitRegister *reg = gnc_ledger_display_get_split_register (gsr->ledger);
    VirtualCellLocation vcell_loc;
    Split *blank;

    ENTER("gsr=%p", gsr);

    blank = gnc_split_register_get_blank_split (reg);
    if (blank == NULL)
    {
        LEAVE("no blank split");
        return;
    }

    if (gnc_split_register_get_split_virt_loc (reg, blank, &vcell_loc))
        gnucash_register_goto_virt_cell (gsr->reg, vcell_loc);

    gnc_ledger_display_refresh (gsr->ledger);
    LEAVE(" ");
}

void
gnc_split_reg_balancing_entry(GNCSplitReg *gsr, Account *account,
                              time_t statement_date, gnc_numeric balancing_amount)
{

    Transaction *transaction;
    Split *split;

    // create transaction
    transaction = create_balancing_transaction(gnc_get_current_book(),
                  account, statement_date, balancing_amount);

    // jump to transaction
    split = xaccTransFindSplitByAccount(transaction, account);
    if (split == NULL)
    {
        // default behaviour: jump to blank split
        g_warning("create_balancing_transaction failed");
        gnc_split_reg_jump_to_blank(gsr);
    }
    else
    {
        // goto balancing transaction
        gnc_split_reg_jump_to_split(gsr, split );
    }
}

static Transaction*
create_balancing_transaction(QofBook *book, Account *account,
                             time_t statement_date, gnc_numeric balancing_amount)
{

    Transaction *trans;
    Split *split;

    if (!account)
        return NULL;
    if (gnc_numeric_zero_p(balancing_amount))
        return NULL;

    xaccAccountBeginEdit(account);

    trans = xaccMallocTransaction(book);

    xaccTransBeginEdit(trans);

    // fill Transaction
    xaccTransSetCurrency(trans, xaccAccountGetCommodity(account));
    xaccTransSetDateSecs(trans, statement_date);
    xaccTransSetDescription(trans, _("Balancing entry from reconcilation"));

    // 1. Split
    split = xaccMallocSplit(book);
    xaccTransAppendSplit(trans, split);
    xaccAccountInsertSplit(account, split);
    xaccSplitSetAmount(split, balancing_amount);
    xaccSplitSetValue(split, balancing_amount);

    // 2. Split (no account is defined: split goes to orphan account)
    split = xaccMallocSplit(book);
    xaccTransAppendSplit(trans, split);

    balancing_amount = gnc_numeric_neg(balancing_amount);
    xaccSplitSetAmount(split, balancing_amount);
    xaccSplitSetValue(split, balancing_amount);

    xaccTransCommitEdit(trans);
    xaccAccountCommitEdit(account);
    return trans;
}

void
gsr_default_blank_handler( GNCSplitReg *gsr, gpointer data )
{
    SplitRegister *reg;

    ENTER("gsr=%p, gpointer=%p", gsr, data);

    reg = gnc_ledger_display_get_split_register (gsr->ledger);

    if (gnc_split_register_save (reg, TRUE))
        gnc_split_register_redraw (reg);

    gnc_split_reg_jump_to_blank (gsr);
    LEAVE(" ");
}

void
gnc_split_reg_new_trans_cb (GtkWidget *widget, gpointer data)
{
    GNCSplitReg *gsr = data;
    gsr_emit_simple_signal( gsr, "blank" );
}

void
gsr_default_jump_handler( GNCSplitReg *gsr, gpointer data )
{
    g_assert_not_reached();
}

void
gnc_split_reg_jump_cb( GtkWidget *widget, gpointer data )
{
    GNCSplitReg *gsr = data;
    gsr_emit_simple_signal( gsr, "jump" );
}

void
gnc_split_reg_change_style (GNCSplitReg *gsr, SplitRegisterStyle style)
{
    SplitRegister *reg = gnc_ledger_display_get_split_register (gsr->ledger);

    if (style == reg->style)
        return;

    gnc_split_register_config (reg, reg->type, style, reg->use_double_line);
    gnc_ledger_display_refresh (gsr->ledger);
}

void
gnc_split_reg_style_ledger_cb (GtkWidget *w, gpointer data)
{
    GNCSplitReg *gsr = data;

    if (!GTK_CHECK_MENU_ITEM(w)->active)
        return;

    gnc_split_reg_change_style (gsr, REG_STYLE_LEDGER);
}

void
gnc_split_reg_style_auto_ledger_cb (GtkWidget *w, gpointer data)
{
    GNCSplitReg *gsr = data;

    if (!GTK_CHECK_MENU_ITEM(w)->active)
        return;

    gnc_split_reg_change_style (gsr, REG_STYLE_AUTO_LEDGER);
}

void
gnc_split_reg_style_journal_cb (GtkWidget *w, gpointer data)
{
    GNCSplitReg *gsr = data;

    if (!GTK_CHECK_MENU_ITEM(w)->active)
        return;

    gnc_split_reg_change_style (gsr, REG_STYLE_JOURNAL);
}

void
gnc_split_reg_double_line_cb (GtkWidget *w, gpointer data)
{
    GNCSplitReg *gsr = data;
    SplitRegister *reg = gnc_ledger_display_get_split_register (gsr->ledger);
    gboolean use_double_line;

    use_double_line = GTK_CHECK_MENU_ITEM(w)->active;
    if ( use_double_line == reg->use_double_line )
        return;

    gnc_split_register_config( reg, reg->type, reg->style, use_double_line );
    gnc_ledger_display_refresh( gsr->ledger );
}

static void
gnc_split_reg_sort( GNCSplitReg *gsr, SortType sort_code )
{
    Query *query = gnc_ledger_display_get_query( gsr->ledger );
    gboolean show_present_divider = FALSE;
    GSList *p1 = NULL, *p2 = NULL, *p3 = NULL, *standard;
    SplitRegister *reg;

    if (gsr->sort_type == sort_code)
        return;

    standard = g_slist_prepend( NULL, QUERY_DEFAULT_SORT );

    switch (sort_code)
    {
    case BY_STANDARD:
        p1 = standard;
        show_present_divider = TRUE;
        break;
    case BY_DATE:
        p1 = g_slist_prepend (p1, TRANS_DATE_POSTED);
        p1 = g_slist_prepend (p1, SPLIT_TRANS);
        p2 = standard;
        show_present_divider = TRUE;
        break;
    case BY_DATE_ENTERED:
        p1 = g_slist_prepend (p1, TRANS_DATE_ENTERED);
        p1 = g_slist_prepend (p1, SPLIT_TRANS);
        p2 = standard;
        break;
    case BY_DATE_RECONCILED:
        p1 = g_slist_prepend (p1, SPLIT_RECONCILE);
        p2 = g_slist_prepend (p2, SPLIT_DATE_RECONCILED);
        p3 = standard;
        break;
    case BY_NUM:
        p1 = g_slist_prepend (p1, TRANS_NUM);
        p1 = g_slist_prepend (p1, SPLIT_TRANS);
        p2 = standard;
        break;
    case BY_AMOUNT:
        p1 = g_slist_prepend (p1, SPLIT_VALUE);
        p2 = standard;
        break;
    case BY_MEMO:
        p1 = g_slist_prepend (p1, SPLIT_MEMO);
        p2 = standard;
        break;
    case BY_DESC:
        p1 = g_slist_prepend (p1, TRANS_DESCRIPTION);
        p1 = g_slist_prepend (p1, SPLIT_TRANS);
        p2 = standard;
        break;
    case BY_ACTION:
        p1 = g_slist_prepend (p1, SPLIT_ACTION);
        p2 = standard;
        break;
    case BY_NOTES:
        p1 = g_slist_prepend (p1, TRANS_NOTES);
        p1 = g_slist_prepend (p1, SPLIT_TRANS);
        p2 = standard;
        break;
    default:
        g_slist_free (standard);
        g_return_if_fail (FALSE);
    }

    gncQuerySetSortOrder( query, p1, p2, p3 );
    reg = gnc_ledger_display_get_split_register( gsr->ledger );
    gnc_split_register_show_present_divider( reg, show_present_divider );
    gsr->sort_type = sort_code;
    gnc_ledger_display_refresh( gsr->ledger );
}

void
gnc_split_reg_sort_standard_cb(GtkWidget *w, gpointer data)
{
    GNCSplitReg *gsr = data;
    gnc_split_reg_sort(gsr, BY_STANDARD);
}

void
gnc_split_reg_sort_date_cb(GtkWidget *w, gpointer data)
{
    GNCSplitReg *gsr = data;
    gnc_split_reg_sort(gsr, BY_DATE);
}

void
gnc_split_reg_sort_date_entered_cb(GtkWidget *w, gpointer data)
{
    GNCSplitReg *gsr = data;
    gnc_split_reg_sort(gsr, BY_DATE_ENTERED);
}

void
gnc_split_reg_sort_date_reconciled_cb(GtkWidget *w, gpointer data)
{
    GNCSplitReg *gsr = data;
    gnc_split_reg_sort(gsr, BY_DATE_RECONCILED);
}

void
gnc_split_reg_sort_num_cb(GtkWidget *w, gpointer data)
{
    GNCSplitReg *gsr = data;
    gnc_split_reg_sort(gsr, BY_NUM);
}

void
gnc_split_reg_sort_amount_cb(GtkWidget *w, gpointer data)
{
    GNCSplitReg *gsr = data;
    gnc_split_reg_sort(gsr, BY_AMOUNT);
}

void
gnc_split_reg_sort_memo_cb(GtkWidget *w, gpointer data)
{
    GNCSplitReg *gsr = data;
    gnc_split_reg_sort(gsr, BY_MEMO);
}

void
gnc_split_reg_sort_desc_cb(GtkWidget *w, gpointer data)
{
    GNCSplitReg *gsr = data;
    gnc_split_reg_sort(gsr, BY_DESC);
}

void
gnc_split_reg_sort_action_cb(GtkWidget *w, gpointer data)
{
    GNCSplitReg *gsr = data;
    gnc_split_reg_sort(gsr, BY_ACTION);
}

void
gnc_split_reg_sort_notes_cb(GtkWidget *w, gpointer data)
{
    GNCSplitReg *gsr = data;
    gnc_split_reg_sort(gsr, BY_NOTES);
}

void
gnc_split_reg_handle_exchange_cb (GtkWidget *w, gpointer data)
{
    GNCSplitReg *gsr = data;
    SplitRegister *reg = gnc_ledger_display_get_split_register (gsr->ledger);

    /* XXX Ignore the return value -- we don't care if this succeeds */
    (void)gnc_split_register_handle_exchange (reg, TRUE);
}

static void
gnc_split_reg_record (GNCSplitReg *gsr)
{
    SplitRegister *reg;
    Transaction *trans;

    ENTER("gsr=%p", gsr);

    reg = gnc_ledger_display_get_split_register (gsr->ledger);
    trans = gnc_split_register_get_current_trans (reg);

    if (!gnc_split_register_save (reg, TRUE))
    {
        LEAVE("no save");
        return;
    }

    gsr_emit_include_date_signal( gsr, xaccTransGetDate(trans) );

    /* Explicit redraw shouldn't be needed,
     * since gui_refresh events should handle this. */
    /* gnc_split_register_redraw (reg); */
    LEAVE(" ");
}

static gboolean
gnc_split_reg_match_trans_row( VirtualLocation virt_loc,
                               gpointer user_data )
{
    GNCSplitReg *gsr = user_data;
    CursorClass cursor_class;
    SplitRegister *sr;

    sr = gnc_ledger_display_get_split_register (gsr->ledger);
    cursor_class = gnc_split_register_get_cursor_class (sr, virt_loc.vcell_loc);

    return (cursor_class == CURSOR_CLASS_TRANS);
}

static void
gnc_split_reg_goto_next_trans_row (GNCSplitReg *gsr)
{
    ENTER("gsr=%p", gsr);
    gnucash_register_goto_next_matching_row( gsr->reg,
            gnc_split_reg_match_trans_row,
            gsr );
    LEAVE(" ");
}

void
gnc_split_reg_enter( GNCSplitReg *gsr, gboolean next_transaction )
{
    SplitRegister *sr = gnc_ledger_display_get_split_register( gsr->ledger );
    gboolean goto_blank;

    ENTER("gsr=%p, next_transaction=%s", gsr, next_transaction ? "TRUE" : "FALSE");

    goto_blank = gnc_gconf_get_bool(GCONF_GENERAL_REGISTER,
                                    "enter_moves_to_end", NULL);

    /* If we are in single or double line mode and we hit enter
     * on the blank split, go to the blank split instead of the
     * next row. This prevents the cursor from jumping around
     * when you are entering transactions. */
    if ( !goto_blank && !next_transaction )
    {
        SplitRegisterStyle style = sr->style;

        if (style == REG_STYLE_LEDGER)
        {
            Split *blank_split;

            blank_split = gnc_split_register_get_blank_split(sr);
            if (blank_split != NULL)
            {
                Split *current_split;

                current_split = gnc_split_register_get_current_split(sr);

                if (blank_split == current_split)
                    goto_blank = TRUE;
            }
        }
    }

    /* First record the transaction. This will perform a refresh. */
    gnc_split_reg_record( gsr );

    if (!goto_blank && next_transaction)
        gnc_split_register_expand_current_trans (sr, FALSE);

    /* Now move. */
    if (goto_blank)
        gnc_split_reg_jump_to_blank( gsr );
    else if (next_transaction)
        gnc_split_reg_goto_next_trans_row( gsr );
    else
        gnucash_register_goto_next_virt_row( gsr->reg );
    LEAVE(" ");
}

void
gsr_default_enter_handler( GNCSplitReg *gsr, gpointer data )
{
    gnc_split_reg_enter( gsr, FALSE );
}

void
gnc_split_reg_record_cb (GnucashRegister *reg, gpointer data)
{
    gsr_emit_simple_signal( (GNCSplitReg*)data, "enter_ent" );
}

void
gnc_split_reg_size_allocate (GtkWidget *widget,
                             GtkAllocation *allocation,
                             gpointer user_data)
{
    GNCSplitReg *gsr = user_data;
    gsr->width = allocation->width;
    gtk_window_set_default_size( GTK_WINDOW(gsr->window), gsr->width, 0 );
}

static
GtkWidget*
add_summary_label (GtkWidget *summarybar, const char *label_str)
{
    GtkWidget *hbox;
    GtkWidget *label;

    hbox = gtk_hbox_new(FALSE, 2);
    gtk_box_pack_start( GTK_BOX(summarybar), hbox, FALSE, FALSE, 5 );

    label = gtk_label_new( label_str );
    gtk_misc_set_alignment( GTK_MISC(label), 1.0, 0.5 );
    gtk_box_pack_start( GTK_BOX(hbox), label, FALSE, FALSE, 0 );

    label = gtk_label_new( "" );
    gtk_misc_set_alignment( GTK_MISC(label), 1.0, 0.5 );
    gtk_box_pack_start( GTK_BOX(hbox), label, FALSE, FALSE, 0 );

    return label;
}

GtkWidget *
gsr_create_summary_bar( GNCSplitReg *gsr )
{
    GtkWidget *summarybar;

    gsr->cleared_label    = NULL;
    gsr->balance_label    = NULL;
    gsr->reconciled_label = NULL;
    gsr->future_label     = NULL;
    gsr->projectedminimum_label  = NULL;
    gsr->shares_label     = NULL;
    gsr->value_label      = NULL;

    if ( gnc_ledger_display_type(gsr->ledger) >= LD_SUBACCOUNT )
    {
        gsr->summarybar = NULL;
        return NULL;
    }

    summarybar = gtk_hbox_new (FALSE, 4);

    if (!xaccAccountIsPriced(gnc_ledger_display_leader(gsr->ledger)))
    {
        gsr->balance_label    = add_summary_label (summarybar, _("Present:"));
        gsr->future_label     = add_summary_label (summarybar, _("Future:"));
        gsr->cleared_label    = add_summary_label (summarybar, _("Cleared:"));
        gsr->reconciled_label = add_summary_label (summarybar, _("Reconciled:"));
        gsr->projectedminimum_label  = add_summary_label (summarybar, _("Projected Minimum:"));
    }
    else
    {
        gsr->shares_label     = add_summary_label (summarybar, _("Shares:"));
        gsr->value_label      = add_summary_label (summarybar, _("Current Value:"));
    }

    gsr->summarybar = summarybar;

    /* Force the first update */
    gsr_redraw_all_cb(NULL, gsr);
    return gsr->summarybar;
}

/**
 * Opens up a register window for a group of Accounts.
 * @param gsr the register window instance
 * @return A GNCPlaceholderType indicating presence and type of placeholder
 * accounts
 **/
static
GNCPlaceholderType
gnc_split_reg_get_placeholder( GNCSplitReg *gsr )
{
    Account *leader;
    SplitRegister *reg;
    gboolean single_account;

    if (gsr == NULL)
        return PLACEHOLDER_NONE;

    reg = gnc_ledger_display_get_split_register( gsr->ledger );

    switch (reg->type)
    {
    case GENERAL_LEDGER:
    case INCOME_LEDGER:
    case PORTFOLIO_LEDGER:
    case SEARCH_LEDGER:
        single_account = FALSE;
        break;
    default:
        single_account = TRUE;
        break;
    }

    leader = gnc_ledger_display_leader( gsr->ledger );

    if (leader == NULL)
        return PLACEHOLDER_NONE;
    if (single_account)
    {
        if (xaccAccountGetPlaceholder( leader ))
            return PLACEHOLDER_THIS;
        return PLACEHOLDER_NONE;
    }
    return xaccAccountGetDescendantPlaceholder( leader );
}

/**
 * @see gtk_callback_bug_workaround
 **/
typedef struct dialog_args
{
    GNCSplitReg *gsr;
    gchar *string;
} dialog_args;

/**
 * Gtk has occasional problems with performing function as part of a
 * callback.  This routine gets called via a timer callback to get it out of
 * the data path with the problem.
 **/
static
gboolean
gtk_callback_bug_workaround (gpointer argp)
{
    dialog_args *args = argp;
    const gchar *read_only = _("This account register is read-only.");
    GtkWidget *dialog;

    dialog = gtk_message_dialog_new(GTK_WINDOW(args->gsr->window),
                                    GTK_DIALOG_DESTROY_WITH_PARENT,
                                    GTK_MESSAGE_WARNING,
                                    GTK_BUTTONS_CLOSE,
                                    "%s", read_only);
    gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(dialog),
            "%s", args->string);
    gnc_dialog_run(GTK_DIALOG(dialog), "register_read_only");
    gtk_widget_destroy(dialog);
    g_free(args);
    return FALSE;
}

/**
 * Determines whether this register window should be read-only.
 **/
static
void
gnc_split_reg_determine_read_only( GNCSplitReg *gsr )
{
    dialog_args *args = g_malloc(sizeof(dialog_args));
    SplitRegister *reg;

    if ( !gsr->read_only )
    {

        switch (gnc_split_reg_get_placeholder(gsr))
        {
        case PLACEHOLDER_NONE:
            /* stay as false. */
            return;

        case PLACEHOLDER_THIS:
            args->string = _("This account may not be edited.  If you want "
                             "to edit transactions in this register, please "
                             "open the account options and turn off the "
                             "placeholder checkbox.");
            break;

        default:
            args->string = _("One of the sub-accounts selected may not be "
                             "edited.  If you want to edit transactions in "
                             "this register, please open the sub-account "
                             "options and turn off the placeholder checkbox. "
                             "You may also open an individual account instead "
                             "of a set of accounts.");
            break;
        }
        gsr->read_only = TRUE;
        /* Put up a warning dialog */
        args->gsr = gsr;
        g_timeout_add (250, gtk_callback_bug_workaround, args); /* 0.25 seconds */
    }

    /* Make the contents immutable */
    reg = gnc_ledger_display_get_split_register( gsr->ledger );
    gnc_split_register_set_read_only( reg, TRUE );

}

static
gncUIWidget
gnc_split_reg_get_parent( GNCLedgerDisplay *ledger )
{
    GNCSplitReg *gsr =
        GNC_SPLIT_REG(gnc_ledger_display_get_user_data( ledger ));

    if (gsr == NULL)
        return NULL;

    return gsr->window;
}

static
void
gsr_emit_help_changed( GnucashRegister *reg, gpointer user_data )
{
    gsr_emit_simple_signal( (GNCSplitReg*)user_data, "help-changed" );
}

static
void
gsr_emit_include_date_signal( GNCSplitReg *gsr, time_t date )
{
    g_signal_emit_by_name( gsr, "include-date", date, NULL );
}

static
void
gsr_emit_simple_signal( GNCSplitReg *gsr, const char *sigName )
{
    g_signal_emit_by_name( gsr, sigName, NULL );
}

GnucashRegister*
gnc_split_reg_get_register( GNCSplitReg *gsr )
{
    if ( !gsr )
        return NULL;

    return gsr->reg;
}

SortType
gnc_split_reg_get_sort_type( GNCSplitReg *gsr )
{
    g_assert( gsr );
    return gsr->sort_type;
}

void
gnc_split_reg_set_sort_type( GNCSplitReg *gsr, SortType t )
{
    gnc_split_reg_sort( gsr, t );
}

GtkWidget*
gnc_split_reg_get_summarybar( GNCSplitReg *gsr )
{
    if ( !gsr ) return NULL;
    return gsr->summarybar;
}

gboolean
gnc_split_reg_get_read_only( GNCSplitReg *gsr )
{
    g_assert( gsr );
    return gsr->read_only;
}

void
gnc_split_reg_set_moved_cb( GNCSplitReg *gsr, GFunc cb, gpointer cb_data )
{
    gnucash_register_set_moved_cb (gsr->reg, cb, cb_data);
}
