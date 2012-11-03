/********************************************************************\
 * gnc-split-reg2.c -- A widget for the common register look-n-feel. *
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

#include "gnc-split-reg2.h"

#include "Account.h"
#include "qof.h"
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
#include "gnc-ledger-display2.h"
#include "gnc-pricedb.h"
#include "gnc-ui-util.h"
#include "gnc-ui.h"

#include "gnucash-sheet.h"
#include "table-allgui.h"

#include "gnc-tree-view-split-reg.h"

#include <libguile.h>
#include "dialog-utils.h"

#define GCONF_SECTION "window/pages/register2"

// static QofLogModule log_module = GNC_MOD_SX;
static QofLogModule log_module = GNC_MOD_GUI;

/***** PROTOTYPES ***************************************************/
void gnc_split_reg2_raise( GNCSplitReg2 *gsr ); /*FIXME What this for */

static GtkWidget* add_summary_label( GtkWidget *summarybar,
                                     const char *label_str );

static void gnc_split_reg2_determine_read_only( GNCSplitReg2 *gsr );

static GNCPlaceholderType gnc_split_reg2_get_placeholder( GNCSplitReg2 *gsr );
static GtkWidget *gnc_split_reg2_get_parent( GNCLedgerDisplay2 *ledger );


static void gsr2_create_table( GNCSplitReg2 *gsr );
static void gsr2_setup_table( GNCSplitReg2 *gsr );



static void gsr2_setup_status_widgets( GNCSplitReg2 *gsr );

static void gsr2_update_summary_label( GtkWidget *label,
                                      xaccGetBalanceFn getter,
                                      Account *leader,
                                      GNCPrintAmountInfo print_info,
                                      gnc_commodity *cmdty,
                                      gboolean reverse,
                                      gboolean euroFlag );

static void gsr2_redraw_all_cb (GncTreeViewSplitReg *view, gpointer data);

static void gnc_split_reg2_refresh_toolbar( GNCSplitReg2 *gsr );

static void gnc_split_reg2_ld_destroy( GNCLedgerDisplay2 *ledger );

static Transaction* create_balancing_transaction(QofBook *book, Account *account,
        time_t statement_date, gnc_numeric balancing_amount);

void gsr2_default_enter_handler    ( GNCSplitReg2 *w, gpointer ud );
void gsr2_default_cancel_handler   ( GNCSplitReg2 *w, gpointer ud );
void gsr2_default_delete_handler   ( GNCSplitReg2 *w, gpointer ud );
void gsr2_default_reinit_handler   ( GNCSplitReg2 *w, gpointer ud );
void gsr2_default_dup_handler      ( GNCSplitReg2 *w, gpointer ud );
void gsr2_default_schedule_handler ( GNCSplitReg2 *w, gpointer ud );
void gsr2_default_expand_handler   ( GNCSplitReg2 *w, gpointer ud );
void gsr2_default_blank_handler    ( GNCSplitReg2 *w, gpointer ud );
void gsr2_default_jump_handler     ( GNCSplitReg2 *w, gpointer ud );
void gsr2_default_cut_handler      ( GNCSplitReg2 *w, gpointer ud );
void gsr2_default_cut_txn_handler  ( GNCSplitReg2 *w, gpointer ud );
void gsr2_default_copy_handler     ( GNCSplitReg2 *w, gpointer ud );
void gsr2_default_copy_txn_handler ( GNCSplitReg2 *w, gpointer ud );
void gsr2_default_paste_handler    ( GNCSplitReg2 *w, gpointer ud );
void gsr2_default_paste_txn_handler( GNCSplitReg2 *w, gpointer ud );
void gsr2_default_void_txn_handler ( GNCSplitReg2 *w, gpointer ud );
void gsr2_default_unvoid_txn_handler  ( GNCSplitReg2 *w, gpointer ud );
void gsr2_default_reverse_txn_handler ( GNCSplitReg2 *w, gpointer ud );

static void gsr2_emit_simple_signal( GNCSplitReg2 *gsr, const char *sigName );
static void gsr2_emit_help_changed (GncTreeViewSplitReg *view, gpointer user_data);
static void gsr2_emit_include_date_signal( GNCSplitReg2 *gsr, time_t date );

void gnc_split_reg2_cut_cb(GtkWidget *w, gpointer data);
void gnc_split_reg2_copy_cb(GtkWidget *w, gpointer data);
void gnc_split_reg2_paste_cb(GtkWidget *w, gpointer data);

void gnc_split_reg2_cut_trans_cb(GtkWidget *w, gpointer data);
void gnc_split_reg2_copy_trans_cb(GtkWidget *w, gpointer data);
void gnc_split_reg2_paste_trans_cb(GtkWidget *w, gpointer data);
void gnc_split_reg2_void_trans_cb(GtkWidget *w, gpointer data);
void gnc_split_reg2_unvoid_trans_cb(GtkWidget *w, gpointer data);
void gnc_split_reg2_reverse_trans_cb(GtkWidget *w, gpointer data);

/*FIXME void gnc_split_reg2_record_cb (GncTreeViewSplitReg *view, gpointer data); */
void gnc_split_reg2_reinitialize_trans_cb(GtkWidget *w, gpointer data);
void gnc_split_reg2_delete_trans_cb(GtkWidget *w, gpointer data);
void gnc_split_reg2_duplicate_trans_cb(GtkWidget *w, gpointer data);
void gnc_split_reg2_recur_cb(GtkWidget *w, gpointer data);
void gnc_split_reg2_record_trans_cb(GtkWidget *w, gpointer data);
void gnc_split_reg2_cancel_trans_cb(GtkWidget *w, gpointer data);

void gnc_split_reg2_expand_trans_menu_cb(GtkWidget *widget, gpointer data);
void gnc_split_reg2_expand_trans_toolbar_cb(GtkWidget *widget, gpointer data);
void gnc_split_reg2_new_trans_cb(GtkWidget *widget, gpointer data);
void gnc_split_reg2_jump_cb(GtkWidget *widget, gpointer data);

void gnc_split_reg2_style_ledger_cb (GtkWidget *w, gpointer data);
void gnc_split_reg2_style_auto_ledger_cb (GtkWidget *w, gpointer data);
void gnc_split_reg2_style_journal_cb (GtkWidget *w, gpointer data);
void gnc_split_reg2_double_line_cb (GtkWidget *w, gpointer data);

void gnc_split_reg2_sort_standard_cb (GtkWidget *w, gpointer data);
void gnc_split_reg2_sort_date_cb (GtkWidget *w, gpointer data);
void gnc_split_reg2_sort_date_entered_cb (GtkWidget *w, gpointer data);
void gnc_split_reg2_sort_date_reconciled_cb (GtkWidget *w, gpointer data);
void gnc_split_reg2_sort_num_cb (GtkWidget *w, gpointer data);
void gnc_split_reg2_sort_amount_cb (GtkWidget *w, gpointer data);
void gnc_split_reg2_sort_memo_cb (GtkWidget *w, gpointer data);
void gnc_split_reg2_sort_desc_cb (GtkWidget *w, gpointer data);
void gnc_split_reg2_sort_action_cb (GtkWidget *w, gpointer data);
void gnc_split_reg2_sort_notes_cb (GtkWidget *w, gpointer data);

void gnc_split_reg2_destroy_cb(GtkWidget *widget, gpointer data);
void gnc_split_reg2_size_allocate( GtkWidget *widget,
                                  GtkAllocation *allocation,
                                  gpointer user_data );


void gnc_split_reg2_handle_exchange_cb (GtkWidget *w, gpointer data);

static void gnc_split_reg2_class_init( GNCSplitReg2Class *class );
static void gnc_split_reg2_init( GNCSplitReg2 *gsr );
static void gnc_split_reg2_init2( GNCSplitReg2 *gsr );

void gnc_split_register_size_allocate (GtkWidget *widget,
                                       GtkAllocation *allocation,
                                       gpointer user_data);

#ifdef SKIP
FROM_STRING_FUNC(SortType, ENUM_LIST_SORTTYPE)
AS_STRING_FUNC(SortType, ENUM_LIST_SORTTYPE)
#endif

GType
gnc_split_reg2_get_type( void )
{
    static GType gnc_split_reg2_type = 0;

    if (!gnc_split_reg2_type)
    {
        GTypeInfo type_info =
        {
            sizeof(GNCSplitReg2Class),      /* class_size */
            NULL,   			/* base_init */
            NULL,				/* base_finalize */
            (GClassInitFunc)gnc_split_reg2_class_init,
            NULL,				/* class_finalize */
            NULL,				/* class_data */
            sizeof(GNCSplitReg2),		/* */
            0,				/* n_preallocs */
            (GInstanceInitFunc)gnc_split_reg2_init,
        };

        gnc_split_reg2_type = g_type_register_static( GTK_TYPE_VBOX,
                             "GNCSplitReg2",
                             &type_info, 0 );
    }

    return gnc_split_reg2_type;
}

/* SIGNALS */
enum gnc_split_reg2_signal_enum
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

static guint gnc_split_reg2_signals[LAST_SIGNAL] = { 0 };

static void
gnc_split_reg2_class_init( GNCSplitReg2Class *class )
{
    int i;
    GtkObjectClass *object_class;
    static struct similar_signal_info
    {
        enum gnc_split_reg2_signal_enum s;
        const char *signal_name;
        guint defaultOffset;
    } signals[] =
    {
        { ENTER_ENT_SIGNAL,    "enter_ent",    G_STRUCT_OFFSET( GNCSplitReg2Class, enter_ent_cb ) },
        { CANCEL_ENT_SIGNAL,   "cancel_ent",   G_STRUCT_OFFSET( GNCSplitReg2Class, cancel_ent_cb ) },
        { DELETE_ENT_SIGNAL,   "delete_ent",   G_STRUCT_OFFSET( GNCSplitReg2Class, delete_ent_cb ) },
        { REINIT_ENT_SIGNAL,   "reinit_ent",   G_STRUCT_OFFSET( GNCSplitReg2Class, reinit_ent_cb ) },
        { DUP_ENT_SIGNAL,      "dup_ent",      G_STRUCT_OFFSET( GNCSplitReg2Class, dup_ent_cb ) },
        { SCHEDULE_ENT_SIGNAL, "schedule_ent", G_STRUCT_OFFSET( GNCSplitReg2Class, schedule_ent_cb ) },
        { EXPAND_ENT_SIGNAL,   "expand_ent",   G_STRUCT_OFFSET( GNCSplitReg2Class, expand_ent_cb ) },
        { BLANK_SIGNAL,        "blank",        G_STRUCT_OFFSET( GNCSplitReg2Class, blank_cb ) },
        { JUMP_SIGNAL,         "jump",         G_STRUCT_OFFSET( GNCSplitReg2Class, jump_cb ) },
        { CUT_SIGNAL,          "cut",          G_STRUCT_OFFSET( GNCSplitReg2Class, cut_cb ) },
        { CUT_TXN_SIGNAL,      "cut_txn",      G_STRUCT_OFFSET( GNCSplitReg2Class, cut_txn_cb ) },
        { COPY_SIGNAL,         "copy",         G_STRUCT_OFFSET( GNCSplitReg2Class, copy_cb ) },
        { COPY_TXN_SIGNAL,     "copy_txn",     G_STRUCT_OFFSET( GNCSplitReg2Class, copy_txn_cb ) },
        { PASTE_SIGNAL,        "paste",        G_STRUCT_OFFSET( GNCSplitReg2Class, paste_cb ) },
        { PASTE_TXN_SIGNAL,    "paste_txn",    G_STRUCT_OFFSET( GNCSplitReg2Class, paste_txn_cb ) },
        { VOID_TXN_SIGNAL,     "void_txn",     G_STRUCT_OFFSET( GNCSplitReg2Class, void_txn_cb ) },
        { UNVOID_TXN_SIGNAL,   "unvoid_txn",   G_STRUCT_OFFSET( GNCSplitReg2Class, unvoid_txn_cb ) },
        { REVERSE_TXN_SIGNAL,  "reverse_txn",  G_STRUCT_OFFSET( GNCSplitReg2Class, reverse_txn_cb ) },
        { HELP_CHANGED_SIGNAL, "help-changed", G_STRUCT_OFFSET( GNCSplitReg2Class, help_changed_cb ) },
        { INCLUDE_DATE_SIGNAL, "include-date", G_STRUCT_OFFSET( GNCSplitReg2Class, include_date_cb ) },
        { LAST_SIGNAL, NULL, 0 }
    };

    object_class = (GtkObjectClass*) class;

    for ( i = 0; signals[i].s != INCLUDE_DATE_SIGNAL; i++ )
    {
        gnc_split_reg2_signals[ signals[i].s ] =
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
    gnc_split_reg2_signals[ INCLUDE_DATE_SIGNAL ] =
        g_signal_new( "include-date",
                      G_TYPE_FROM_CLASS(object_class),
                      G_SIGNAL_RUN_LAST,
                      signals[i++].defaultOffset,
                      NULL, NULL,
                      g_cclosure_marshal_VOID__INT, /* time_t == int */
                      G_TYPE_NONE, 1, G_TYPE_INT );

    g_assert( i == LAST_SIGNAL );

    /* Setup the default handlers. */
    class->enter_ent_cb    = gsr2_default_enter_handler;
    class->cancel_ent_cb   = gsr2_default_cancel_handler;
    class->delete_ent_cb   = gsr2_default_delete_handler;
    class->reinit_ent_cb   = gsr2_default_reinit_handler;
    class->dup_ent_cb      = gsr2_default_dup_handler;
    class->schedule_ent_cb = gsr2_default_schedule_handler;
    class->expand_ent_cb   = gsr2_default_expand_handler;
    class->blank_cb        = gsr2_default_blank_handler;
    class->jump_cb         = gsr2_default_jump_handler;
    class->cut_cb          = gsr2_default_cut_handler;
    class->cut_txn_cb      = gsr2_default_cut_txn_handler;
    class->copy_cb         = gsr2_default_copy_handler;
    class->copy_txn_cb     = gsr2_default_copy_txn_handler;
    class->paste_cb        = gsr2_default_paste_handler;
    class->paste_txn_cb    = gsr2_default_paste_txn_handler;
    class->void_txn_cb     = gsr2_default_void_txn_handler;
    class->unvoid_txn_cb   = gsr2_default_unvoid_txn_handler;
    class->reverse_txn_cb  = gsr2_default_reverse_txn_handler;

    class->help_changed_cb = NULL;
    class->include_date_cb = NULL;
}

GtkWidget*
gnc_split_reg2_new( GNCLedgerDisplay2 *ld,
                   GtkWindow *parent,
                   gint numberOfLines,
                   gboolean read_only )
{
    GNCSplitReg2 *gsrToRet;

    ENTER("ld=%p, parent=%p, numberOfLines=%d, read_only=%s",
          ld, parent, numberOfLines, read_only ? "TRUE" : "FALSE");

    gsrToRet = g_object_new( gnc_split_reg2_get_type(), NULL );

    gsrToRet->numRows        = numberOfLines;
    gsrToRet->read_only      = read_only;

    gsrToRet->ledger = ld;
    gsrToRet->window = GTK_WIDGET(parent);

    gnc_split_reg2_init2( gsrToRet );

    LEAVE("%p", gsrToRet);
    return GTK_WIDGET( gsrToRet );
}

static void
gnc_split_reg2_init( GNCSplitReg2 *gsr )
{
    gsr->sort_type = BY_STANDARD;
    gsr->width = -1;
    gsr->height = -1;
    gsr->numRows = 10;
    gsr->read_only = FALSE;

    g_signal_connect( gsr, "destroy",
                      G_CALLBACK (gnc_split_reg2_destroy_cb), gsr );
}

static void
gnc_split_reg2_init2( GNCSplitReg2 *gsr )
{
    if ( !gsr ) return;

    gnc_split_reg2_determine_read_only( gsr );

    gsr2_setup_status_widgets( gsr );
    /* ordering is important here... setup_status before create_table */

    gsr2_create_table( gsr );
    gsr2_setup_table( gsr );

}

static
void
gsr2_setup_table( GNCSplitReg2 *gsr )
{
    GncTreeModelSplitReg *model;

    ENTER("gsr=%p", gsr);

    model = gnc_ledger_display2_get_split_model_register( gsr->ledger );

/*FIXME    gnc_split_register_show_present_divider( model, TRUE ); */

    /* events should be sufficient to redraw this */
    /* gnc_ledger_display2_refresh( gsr->ledger ); */
    gnc_split_reg2_refresh_toolbar( gsr );

    LEAVE(" ");
}

static
void
gsr2_create_table( GNCSplitReg2 *gsr )
{
    GtkWidget *register_widget;
    GncTreeViewSplitReg *view;
    GncTreeModelSplitReg *model;
    GtkWidget *scrolled_window;
    GtkTreeViewColumn *col;

    gchar *gconf_key;
    const GncGUID * guid;
    Account * account;
    
    account = gnc_ledger_display2_leader (gsr->ledger);
    guid = xaccAccountGetGUID (account);
    /* Used for saving different register column widths under seperate keys */
    gconf_key = g_strconcat (GCONF_SECTION,"/", (gchar*)guid_to_string (guid), NULL); 

    ENTER("gsr=%p", gsr);
g_print("gsr2_create_table\n");
    gnc_ledger_display2_set_user_data (gsr->ledger, (gpointer)gsr );
    gnc_ledger_display2_set_handlers (gsr->ledger,
                                     gnc_split_reg2_ld_destroy,
                                     gnc_split_reg2_get_parent );

    model = gnc_ledger_display2_get_split_model_register (gsr->ledger );
    view = gnc_tree_view_split_reg_new_with_model (model);

    g_object_unref(G_OBJECT(model));

    scrolled_window = gtk_scrolled_window_new (NULL, NULL);
    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window),
                                   GTK_POLICY_AUTOMATIC,
                                   GTK_POLICY_AUTOMATIC);
    gtk_widget_show (scrolled_window);
    gtk_box_pack_start (GTK_BOX (gsr), scrolled_window, TRUE, TRUE, 0);

    gnc_ledger_display2_set_split_view_register (gsr->ledger, view);

    g_object_set (G_OBJECT (view), "gconf-section", gconf_key, 
                 "show-column-menu", TRUE, NULL);

    gnc_tree_view_configure_columns (GNC_TREE_VIEW (view));

    gnc_tree_view_set_show_column_menu (GNC_TREE_VIEW (view), FALSE);

    /* This column gets all the free space */
    gnc_tree_view_expand_columns (GNC_TREE_VIEW (view), "descnotes", NULL);

    /*FIXME is this OK ? - Set the Reconcile column width */
    col = gnc_tree_view_find_column_by_name (GNC_TREE_VIEW (view), "recn");
    if (col != NULL)
        g_object_set (G_OBJECT(col),
                     "resizable", FALSE,
                     "sizing",      GTK_TREE_VIEW_COLUMN_FIXED,
                     "fixed-width", 15,
                     NULL);

    /* This sets the right color column, 4 is the minimum */
    col = gnc_tree_view_find_column_by_name (GNC_TREE_VIEW (view), "status");
    if (col != NULL)
        g_object_set (G_OBJECT(col),
                     "resizable", FALSE,
                     "sizing",      GTK_TREE_VIEW_COLUMN_FIXED,
                     "fixed-width", 4,
                     NULL);

    gtk_tree_view_set_headers_visible (GTK_TREE_VIEW (view), TRUE);
    gtk_widget_show (GTK_WIDGET (view));
    gtk_container_add (GTK_CONTAINER (scrolled_window), GTK_WIDGET (view));
    gtk_widget_show (GTK_WIDGET (gsr));

    /* Should this be read only */
    gnc_tree_view_split_reg_set_read_only (view, gsr->read_only);

    /* This tells the ledger that we have a valid tree view */
    gnc_ledger_display2_set_split_view_refresh (gsr->ledger, TRUE);

/*
    g_signal_connect (gsr->reg, "activate_cursor",
                      G_CALLBACK(gnc_split_reg2_record_cb), gsr); */

/*FIXME This triggers the update of the summary bar, we need to connect this to some thing

    g_signal_connect (gsr->reg, "redraw_all",
                      G_CALLBACK(gsr2_redraw_all_cb), gsr); */


    g_signal_connect (view, "help_signal",
                      G_CALLBACK (gsr2_emit_help_changed), gsr); // this works

    LEAVE(" ");
}

static
void
gsr2_setup_status_widgets (GNCSplitReg2 *gsr)
{
    GncTreeModelSplitReg *model;
    gboolean use_double_line;

    model = gnc_ledger_display2_get_split_model_register (gsr->ledger);
    use_double_line = gnc_ledger_display2_default_double_line (gsr->ledger);

    /* be sure to initialize the gui elements */
    gnc_tree_model_split_reg_config (model, model->type, model->style, use_double_line);
}

void
gnc_split_reg2_destroy_cb (GtkWidget *widget, gpointer data)
{
//g_print("gnc_split_reg2_destroy_cb - does not do any thing\n");
}

/**
 * Raise an existing register window to the front.
 **/
void
gnc_split_reg2_raise( GNCSplitReg2 *gsr )
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
gsr2_update_summary_label (GtkWidget *label,
                          xaccGetBalanceFn getter,
                          Account *leader,
                          GNCPrintAmountInfo print_info,
                          gnc_commodity *cmdty,
                          gboolean reverse,
                          gboolean euroFlag)
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
    pdb = gnc_pricedb_get_db (book);

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
    pdb = gnc_pricedb_get_db (book);

    price_list = gnc_pricedb_lookup_latest_any_currency (pdb, commodity);
    if (!price_list) return NULL;

    result = gnc_price_clone ((GNCPrice *)(price_list->data), book);

    gnc_price_list_destroy (price_list);

    return result;
}

static
void
gsr2_redraw_all_cb (GncTreeViewSplitReg *view, gpointer user_data)
{
    GNCSplitReg2 *gsr = user_data;
    gnc_commodity * commodity;
    GNCPrintAmountInfo print_info;
    gnc_numeric amount;
    char string[256];
    Account *leader;
    gboolean reverse;
    gboolean euro;

    if ( gsr->summarybar == NULL )
        return;

    leader = gnc_ledger_display2_leader( gsr->ledger );

    commodity = xaccAccountGetCommodity( leader );

    /* no EURO converson, if account is already EURO or no EURO currency */
    if (commodity != NULL)
        euro = (gnc_is_euro_currency( commodity ) &&
                (strncasecmp(gnc_commodity_get_mnemonic(commodity), "EUR", 3)));
    else
        euro = FALSE;

    print_info = gnc_account_print_info( leader, TRUE );
    reverse = gnc_reverse_balance( leader );

    gsr2_update_summary_label( gsr->balance_label,
                              xaccAccountGetPresentBalance,
                              leader, print_info, commodity, reverse, euro );
    gsr2_update_summary_label( gsr->cleared_label,
                              xaccAccountGetClearedBalance,
                              leader, print_info, commodity, reverse, euro );
    gsr2_update_summary_label( gsr->reconciled_label,
                              xaccAccountGetReconciledBalance,
                              leader, print_info, commodity, reverse, euro );
    gsr2_update_summary_label( gsr->future_label,
                              xaccAccountGetBalance,
                              leader, print_info, commodity, reverse, euro );
    gsr2_update_summary_label( gsr->projectedminimum_label,
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
                                      GNC_HOW_RND_ROUND_HALF_UP);

            xaccSPrintAmount (string, amount, print_info);

            gnc_set_label_color (gsr->value_label, amount);
            gtk_label_set_text (GTK_LABEL (gsr->value_label), string);

            gnc_price_unref (price);
        }
    }

}

static void
gnc_split_reg2_refresh_toolbar( GNCSplitReg2 *gsr )
{
    GtkToolbarStyle tbstyle;

    if ((gsr == NULL) || (gsr->toolbar == NULL))
        return;

    tbstyle = gnc_get_toolbar_style ();
    gtk_toolbar_set_style( GTK_TOOLBAR(gsr->toolbar), tbstyle );
}

static void
gnc_split_reg2_ld_destroy( GNCLedgerDisplay2 *ledger )
{
    GNCSplitReg2 *gsr = gnc_ledger_display2_get_user_data( ledger );
    
    gchar *gconf_key;
    const GncGUID * guid;
    Account * account;
    
    account = gnc_ledger_display2_leader(ledger);
    guid = xaccAccountGetGUID(account);
    gconf_key = (gchar*)guid_to_string (guid);
    
    
    if (gsr)
    {
        GncTreeModelSplitReg *model;

        model = gnc_ledger_display2_get_split_model_register (ledger);

/*FIXME        if (model && model->table)
            gnc_table_save_state (model->table, gconf_key);
*/
        /*
         * Don't destroy the window here any more.  The register no longer
         * owns it.
         */
    }
    gnc_ledger_display2_set_user_data (ledger, NULL);
}


/* ######## Handlers ############ */


void
gsr2_default_cut_handler( GNCSplitReg2 *gsr, gpointer data )
{
/*FIXME    gnucash_register_cut_clipboard( gsr->reg );*/
}

/**
 * Cut the selection to the clipboard.  This refers to the Split.
 **/
void
gnc_split_reg2_cut_cb (GtkWidget *w, gpointer data)
{
/*FIXME    GNCSplitReg2 *gsr = data;
    gsr2_emit_simple_signal( gsr, "cut" );*/
}

void
gsr2_default_copy_handler( GNCSplitReg2 *gsr, gpointer data )
{
/*FIXME    gnucash_register_copy_clipboard( gsr->reg );*/
}

/**
 * Copy the selection to the clipboard.  This refers to the Split.
 **/
void
gnc_split_reg2_copy_cb (GtkWidget *w, gpointer data)
{
/*FIXME    GNCSplitReg2 *gsr = data;
    gsr2_emit_simple_signal( gsr, "copy" );*/
}

void
gsr2_default_paste_handler( GNCSplitReg2 *gsr, gpointer data )
{
/*FIXME    gnucash_register_paste_clipboard( gsr->reg );*/
}

/**
 * Paste the clipboard to the selection.  This refers to the Split.
 **/
void
gnc_split_reg2_paste_cb (GtkWidget *w, gpointer data)
{
/*FIXME    GNCSplitReg2 *gsr = data;
    gsr2_emit_simple_signal( gsr, "paste" );*/
}

void
gsr2_default_cut_txn_handler( GNCSplitReg2 *gsr, gpointer data )
{
/*FIXME    gnc_split_register_cut_current
    (gnc_ledger_display2_get_split_register( gsr->ledger ));*/
}

/**
 * Cut the current transaction  to the clipboard.
 **/
void
gnc_split_reg2_cut_trans_cb (GtkWidget *w, gpointer data)
{
/*FIXME    GNCSplitReg2 *gsr = data;
    gsr2_emit_simple_signal( gsr, "cut_txn" );*/
}

void
gsr2_default_copy_txn_handler( GNCSplitReg2 *gsr, gpointer data )
{
/*FIXME    gnc_split_register_copy_current
    (gnc_ledger_display2_get_split_register( gsr->ledger ));*/
}

/**
 * Copy the current transaction to the clipboard.
 **/
void
gnc_split_reg2_copy_trans_cb(GtkWidget *w, gpointer data)
{
/*FIXME    GNCSplitReg2 *gsr = data;
    gsr2_emit_simple_signal( gsr, "copy_txn" );*/
}

void
gsr2_default_paste_txn_handler( GNCSplitReg2 *gsr, gpointer data )
{
/*FIXME    gnc_split_register_paste_current
    (gnc_ledger_display2_get_split_register( gsr->ledger ));*/
}

/**
 * Paste the transaction clipboard to the selection.
 **/
void
gnc_split_reg2_paste_trans_cb (GtkWidget *w, gpointer data)
{
/*FIXME    GNCSplitReg2 *gsr = data;
    gsr2_emit_simple_signal( gsr, "paste_txn" );*/
}

/********************************************************************\
 * gnc_split_reg2_void_trans_cb                                      *
 *                                                                  *
 * Args:   widget - the widget that called us                       *
 *         data   - the data struct for this register               *
 * Return: none                                                     *
\********************************************************************/
void
gsr2_default_void_txn_handler (GNCSplitReg2 *gsr, gpointer data)
{
    // Override this function.
}

void
gnc_split_reg2_void_trans_cb (GtkWidget *w, gpointer data)
{
/*FIXME    GNCSplitReg2 *gsr = data;
    gsr2_emit_simple_signal( gsr, "void_txn" );*/
}

/********************************************************************\
 * gnc_split_reg2_unvoid_trans_cb                                      *
 *                                                                  *
 * Args:   widget - the widget that called us                       *
 *         data   - the data struct for this register               *
 * Return: none                                                     *
\********************************************************************/
void
gsr2_default_unvoid_txn_handler (GNCSplitReg2 *gsr, gpointer data)
{
    // Override this function.
}

void
gnc_split_reg2_unvoid_trans_cb (GtkWidget *w, gpointer data)
{
/*FIXME    GNCSplitReg2 *gsr = data;
    gsr2_emit_simple_signal( gsr, "unvoid_txn" );*/
}

/********************************************************************\
 * gnc_split_reg2_reverse_trans_cb                                   *
 *                                                                  *
 * Args:   widget - the widget that called us                       *
 *         data   - the data struct for this register               *
 * Return: none                                                     *
\********************************************************************/
void
gsr2_default_reverse_txn_handler (GNCSplitReg2 *gsr, gpointer data)
{
/*FIXME*/
#ifdef skip
    SplitRegister *reg;
    Transaction *trans, *new_trans;

    reg = gnc_ledger_display2_get_split_register( gsr->ledger );
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
    gnc_split_reg2_jump_to_split(gsr, xaccTransGetSplit(new_trans, 0));
#endif
}

void
gnc_split_reg2_reverse_trans_cb (GtkWidget *w, gpointer data)
{
/*FIXME    GNCSplitReg2 *gsr = data;
    gsr2_emit_simple_signal( gsr, "reverse_txn" ); */
}


static gboolean
is_trans_readonly_and_warn (const Transaction *trans) //this works
{
    GtkWidget *dialog;
    const gchar *reason;
    const gchar *title = _("Cannot modify or delete this transaction.");
    const gchar *message =
        _("This transaction is marked read-only with the comment: '%s'");

    if (!trans) return FALSE;

    if (xaccTransIsReadonlyByPostedDate (trans))
    {
        dialog = gtk_message_dialog_new (NULL,
                                        0,
                                        GTK_MESSAGE_ERROR,
                                        GTK_BUTTONS_OK,
                                        "%s", title);
        gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
                "%s", _("The date of this transaction is older than the \"Read-Only Threshold\" set for this book.  "
                        "This setting can be changed in File -> Properties -> Accounts."));
        gtk_dialog_run (GTK_DIALOG (dialog));
        gtk_widget_destroy (dialog);
        return TRUE;
    }

    reason = xaccTransGetReadOnly (trans);
    if (reason)
    {
        dialog = gtk_message_dialog_new (NULL,
                                        0,
                                        GTK_MESSAGE_ERROR,
                                        GTK_BUTTONS_OK,
                                        "%s", title);
        gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
                message, reason);
        gtk_dialog_run (GTK_DIALOG (dialog));
        gtk_widget_destroy (dialog);
        return TRUE;
    }
    return FALSE;
}


void
gsr2_default_reinit_handler ( GNCSplitReg2 *gsr, gpointer data ) //this works
{
    GncTreeViewSplitReg *view;
    Transaction *trans;
    Split *split;
    GtkWidget *dialog;
    gint response;
    const gchar *warning;

    const char *title = _("Remove the splits from this transaction?");
    const char *recn_warn = _("This transaction contains reconciled splits. "
                              "Modifying it is not a good idea because that will "
                              "cause your reconciled balance to be off.");

    view = gnc_ledger_display2_get_split_view_register ( gsr->ledger );

    trans = gnc_tree_view_split_reg_get_current_trans (view);
    if (is_trans_readonly_and_warn (trans))
        return;
    dialog = gtk_message_dialog_new (GTK_WINDOW (gsr->window),
                                    GTK_DIALOG_DESTROY_WITH_PARENT,
                                    GTK_MESSAGE_WARNING,
                                    GTK_BUTTONS_NONE,
                                    "%s", title);
    if (xaccTransHasReconciledSplits (trans))
    {
        gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
                "%s", recn_warn);
        warning = "register_remove_all_splits2";
    }
    else
    {
        warning = "register_remove_all_splits";
    }

    gtk_dialog_add_button (GTK_DIALOG (dialog),
                          GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL);
    gnc_gtk_dialog_add_button(dialog, _("_Remove Splits"),
                              GTK_STOCK_DELETE, GTK_RESPONSE_ACCEPT);
    response = gnc_dialog_run (GTK_DIALOG(dialog), warning);
    gtk_widget_destroy (dialog);
    if (response != GTK_RESPONSE_ACCEPT)
        return;

    gnc_tree_view_split_reg_reinit_trans (view);
}

/**
 * "Reinitializes" the current transaction.
 **/
void
gnc_split_reg2_reinitialize_trans_cb(GtkWidget *widget, gpointer data)
{
/*FIXME    GNCSplitReg2 *gsr = data;
    gsr2_emit_simple_signal( gsr, "reinit_ent" ); */
}

void
gsr2_default_delete_handler (GNCSplitReg2 *gsr, gpointer data) //this works
{
    GncTreeViewSplitReg *view;
    RowDepth depth;
    Transaction *trans;
    Split *split;
    GtkWidget *dialog;
    gint response;
    const gchar *warning;

    view = gnc_ledger_display2_get_split_view_register (gsr->ledger);

    /* get the current split based on cursor position */
    split = gnc_tree_view_split_reg_get_current_split (view);
    if (split == NULL)
    {
        return;
    }

    trans = xaccSplitGetParent (split);
    depth = gnc_tree_view_reg_get_selected_row_depth (view);

    /* Deleting the blank split just cancels */
    {
        Split *blank_split = gnc_tree_view_split_reg_get_blank_split (view);

        if (split == blank_split)
        {
            return;
        }
    }

    if (is_trans_readonly_and_warn (trans))
        return;

    /* On a split cursor, just delete the one split. */
    if (depth == SPLIT3)
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

        if (split == gnc_tree_view_reg_get_current_trans_split (view))
        {
            dialog = gtk_message_dialog_new (GTK_WINDOW (gsr->window),
                                            GTK_DIALOG_MODAL
                                            | GTK_DIALOG_DESTROY_WITH_PARENT,
                                            GTK_MESSAGE_ERROR,
                                            GTK_BUTTONS_OK,
                                            "%s", anchor_error);
            gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
                    "%s", anchor_split);
            gtk_dialog_run (GTK_DIALOG (dialog));
            gtk_widget_destroy (dialog);
            return;
        }

        memo = xaccSplitGetMemo (split);
        memo = (memo && *memo) ? memo : _("(no memo)");

        desc = xaccTransGetDescription (trans);
        desc = (desc && *desc) ? desc : _("(no description)");

        /* ask for user confirmation before performing permanent damage */
        buf = g_strdup_printf (format, memo, desc);
        dialog = gtk_message_dialog_new (GTK_WINDOW (gsr->window),
                                        GTK_DIALOG_MODAL
                                        | GTK_DIALOG_DESTROY_WITH_PARENT,
                                        GTK_MESSAGE_QUESTION,
                                        GTK_BUTTONS_NONE,
                                        "%s", buf);
        g_free(buf);
        recn = xaccSplitGetReconcile (split);
        if (recn == YREC || recn == FREC)
        {
            gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
                    "%s", recn_warn);
            warning = "register_delete_split2";
        }
        else
        {
            warning = "register_delete_split";
        }

        gtk_dialog_add_button (GTK_DIALOG (dialog),
                              GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL);
        gnc_gtk_dialog_add_button (dialog, _("_Delete Split"),
                                  GTK_STOCK_DELETE, GTK_RESPONSE_ACCEPT);
        response = gnc_dialog_run (GTK_DIALOG (dialog), warning);
        gtk_widget_destroy (dialog);
        if (response != GTK_RESPONSE_ACCEPT)
            return;

        gnc_tree_view_split_reg_delete_current_split (view);
        return;
    }

    g_return_if_fail (depth == TRANS1 || depth == TRANS2);

    /* On a transaction cursor with 2 or fewer splits in single or double
     * mode, we just delete the whole transaction, kerblooie */
    {
        const char *title = _("Delete the current transaction?");
        const char *recn_warn = _("You would be deleting a transaction "
                                  "with reconciled splits! "
                                  "This is not a good idea as it will cause your "
                                  "reconciled balance to be off.");

        dialog = gtk_message_dialog_new (GTK_WINDOW (gsr->window),
                                        GTK_DIALOG_MODAL
                                        | GTK_DIALOG_DESTROY_WITH_PARENT,
                                        GTK_MESSAGE_WARNING,
                                        GTK_BUTTONS_NONE,
                                        "%s", title);
        if (xaccTransHasReconciledSplits (trans))
        {
            gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
                    "%s", recn_warn);
            warning = "register_delete_trans2";
        }
        else
        {
            warning = "register_delete_trans";
        }
        gtk_dialog_add_button (GTK_DIALOG (dialog),
                              GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL);
        gnc_gtk_dialog_add_button (dialog, _("_Delete Transaction"),
                                  GTK_STOCK_DELETE, GTK_RESPONSE_ACCEPT);
        response =  gnc_dialog_run (GTK_DIALOG (dialog), warning);
        gtk_widget_destroy (dialog);
        if (response != GTK_RESPONSE_ACCEPT)
            return;

        gnc_tree_view_split_reg_delete_current_trans (view);
        return;
    }
}

/**
 * Deletes the current transaction.
 **/
void
gnc_split_reg2_delete_trans_cb(GtkWidget *widget, gpointer data)
{
/*FIXME    GNCSplitReg2 *gsr = data;
    gsr2_emit_simple_signal( gsr, "delete_ent" );*/
}


void
gsr2_default_dup_handler( GNCSplitReg2 *gsr, gpointer data )
{
/*FIXME    gnc_split_register_duplicate_current
    (gnc_ledger_display2_get_split_register( gsr->ledger ));*/
}


/**
 * Duplicates the current transaction in the register.
 **/
void
gnc_split_reg2_duplicate_trans_cb(GtkWidget *w, gpointer data)
{
/*FIXME    GNCSplitReg2 *gsr = data;
    gsr2_emit_simple_signal( gsr, "dup_ent" );*/
}

/**
 * Schedules the current transaction for recurring-entry.
 * If the selected transaction was created from a scheduled transaction,
 * opens the editor for that Scheduled Transaction.
 **/
void
gsr2_default_schedule_handler( GNCSplitReg2 *gsr, gpointer data )
{
/*FIXME*/
#ifdef skip
    SplitRegister *reg = gnc_ledger_display2_get_split_register( gsr->ledger );
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
#endif
}

void
gnc_split_reg2_recur_cb(GtkWidget *w, gpointer data)
{
/*FIXME    GNCSplitReg2 *gsr = data;
    gsr2_emit_simple_signal( gsr, "schedule_ent" );*/
}

/**
 * Records into the books the currently-selected transaction.
 **/
void
gnc_split_reg2_record_trans_cb (GtkWidget *w, gpointer data)
{
/*FIXME    GNCSplitReg2 *gsr = data;
    gsr2_emit_simple_signal( gsr, "enter_ent" );*/
}

void
gsr2_default_cancel_handler( GNCSplitReg2 *gsr, gpointer data )
{
/*FIXME    gnc_split_register_cancel_cursor_trans_changes
    (gnc_ledger_display2_get_split_register( gsr->ledger ));*/
}

/**
 * Cancels the edits of the currently-selected transaction.
 **/
void
gnc_split_reg2_cancel_trans_cb(GtkWidget *w, gpointer data)
{
/*FIXME    GNCSplitReg2 *gsr = data;
    gsr2_emit_simple_signal( gsr, "cancel_ent" );*/
}

void
gsr2_default_expand_handler( GNCSplitReg2 *gsr, gpointer data )
{
/*FIXME*/
#ifdef skip
    gint activeCount;
    gboolean expand;
    SplitRegister *reg;

    if (!gsr)
        return;

    reg = gnc_ledger_display2_get_split_register (gsr->ledger);

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
#endif
}

void
gnc_split_reg2_expand_trans_menu_cb (GtkWidget *widget, gpointer data)
{
/*FIXME    GNCSplitReg2 *gsr = data;
    gsr2_emit_simple_signal( gsr, "expand_ent" );*/
}

void
gnc_split_reg2_expand_trans_toolbar_cb (GtkWidget *widget, gpointer data)
{
/*FIXME    GNCSplitReg2 *gsr = data;
    gsr2_emit_simple_signal( gsr, "expand_ent" );*/
}

/**
 * move the cursor to the split, if present in register
**/
void
gnc_split_reg2_jump_to_split (GNCSplitReg2 *gsr, Split *split) //this works
{
    GncTreeViewSplitReg *view;

//FIXME maybe collapse this back to plugin-page-register

    view = gnc_ledger_display2_get_split_view_register (gsr->ledger);

    gnc_tree_view_split_reg_jump_to_split (view, split);

}


/**
 * Move the cursor to the split in the non-blank amount column.
 **/
void
gnc_split_reg2_jump_to_split_amount(GNCSplitReg2 *gsr, Split *split)
{
/*FIXME*/
#ifdef skip
    VirtualLocation virt_loc;
    SplitRegister *reg;
    Transaction *trans;

    if (!gsr) return;

    trans = xaccSplitGetParent(split);
    gsr2_emit_include_date_signal( gsr, xaccTransGetDate(trans) );

    reg = gnc_ledger_display2_get_split_register (gsr->ledger);

    if (gnc_split_register_get_split_amount_virt_loc (reg, split, &virt_loc))
        gnucash_register_goto_virt_loc (gsr->reg, virt_loc);

    gnc_ledger_display2_refresh (gsr->ledger);
#endif
}

void
gnc_split_reg2_jump_to_blank (GNCSplitReg2 *gsr)
{
/*FIXME*/
#ifdef skip
    SplitRegister *reg = gnc_ledger_display2_get_split_register (gsr->ledger);
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

    gnc_ledger_display2_refresh (gsr->ledger);
    LEAVE(" ");
#endif
}

void
gnc_split_reg2_balancing_entry(GNCSplitReg2 *gsr, Account *account,
                              time_t statement_date, gnc_numeric balancing_amount)
{
/*FIXME*/
#ifdef skip
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
        gnc_split_reg2_jump_to_blank(gsr);
    }
    else
    {
        // goto balancing transaction
        gnc_split_reg2_jump_to_split(gsr, split );
    }
#endif
}

static Transaction*
create_balancing_transaction(QofBook *book, Account *account,
                             time_t statement_date, gnc_numeric balancing_amount)
{
/*FIXME*/
#ifdef skip
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
    xaccTransSetDatePostedSecs(trans, statement_date);
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
#endif
return NULL;
}

void
gsr2_default_blank_handler( GNCSplitReg2 *gsr, gpointer data )
{
/*FIXME*/
#ifdef skip
    SplitRegister *reg;

    ENTER("gsr=%p, gpointer=%p", gsr, data);

    reg = gnc_ledger_display2_get_split_register (gsr->ledger);

    if (gnc_split_register_save (reg, TRUE))
        gnc_split_register_redraw (reg);

    gnc_split_reg2_jump_to_blank (gsr);
    LEAVE(" ");
#endif
}

void
gnc_split_reg2_new_trans_cb (GtkWidget *widget, gpointer data)
{
/*FIXME   GNCSplitReg2 *gsr = data;
    gsr2_emit_simple_signal( gsr, "blank" );*/
}

void
gsr2_default_jump_handler( GNCSplitReg2 *gsr, gpointer data )
{
 /*FIXME   g_assert_not_reached();*/
}

void
gnc_split_reg2_jump_cb( GtkWidget *widget, gpointer data )
{
/*FIXME    GNCSplitReg2 *gsr = data;
    gsr2_emit_simple_signal( gsr, "jump" ); */
}

/* ###### End Handlers ##### */





void
gnc_split_reg2_change_style (GNCSplitReg2 *gsr, SplitRegisterStyle2 style)
{
    GncTreeModelSplitReg *model = gnc_ledger_display2_get_split_model_register (gsr->ledger);

    if (style == model->style)
        return;

    gnc_tree_model_split_reg_config (model, model->type, style, model->use_double_line);
    gnc_ledger_display2_refresh (gsr->ledger);
}

void
gnc_split_reg2_style_ledger_cb (GtkWidget *w, gpointer data)
{
    GNCSplitReg2 *gsr = data;

    if (!GTK_CHECK_MENU_ITEM(w)->active)
        return;

    gnc_split_reg2_change_style (gsr, REG2_STYLE_LEDGER);
}

void
gnc_split_reg2_style_auto_ledger_cb (GtkWidget *w, gpointer data)
{
    GNCSplitReg2 *gsr = data;

    if (!GTK_CHECK_MENU_ITEM(w)->active)
        return;

    gnc_split_reg2_change_style (gsr, REG2_STYLE_AUTO_LEDGER);
}

void
gnc_split_reg2_style_journal_cb (GtkWidget *w, gpointer data)
{
    GNCSplitReg2 *gsr = data;

    if (!GTK_CHECK_MENU_ITEM(w)->active)
        return;

    gnc_split_reg2_change_style (gsr, REG2_STYLE_JOURNAL);
}

void
gnc_split_reg2_double_line_cb (GtkWidget *w, gpointer data)
{
    GNCSplitReg2 *gsr = data;
    GncTreeModelSplitReg *model = gnc_ledger_display2_get_split_model_register (gsr->ledger);
    gboolean use_double_line;

    use_double_line = GTK_CHECK_MENU_ITEM(w)->active;
    if ( use_double_line == model->use_double_line )
        return;

    gnc_tree_model_split_reg_config( model, model->type, model->style, use_double_line );
    gnc_ledger_display2_refresh( gsr->ledger );
}

static void
gnc_split_reg2_sort( GNCSplitReg2 *gsr, SortType sort_code )
{
    Query *query = gnc_ledger_display2_get_query( gsr->ledger );
    gboolean show_present_divider = FALSE;
    GSList *p1 = NULL, *p2 = NULL, *p3 = NULL, *standard;
    GncTreeModelSplitReg *model;

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

    qof_query_set_sort_order( query, p1, p2, p3 );
    model = gnc_ledger_display2_get_split_model_register( gsr->ledger );
/*FIXME    gnc_split_register_show_present_divider( model, show_present_divider ); */
    gsr->sort_type = sort_code;
    gnc_ledger_display2_refresh( gsr->ledger );
}

void
gnc_split_reg2_sort_standard_cb(GtkWidget *w, gpointer data)
{
    GNCSplitReg2 *gsr = data;
    gnc_split_reg2_sort(gsr, BY_STANDARD);
}

void
gnc_split_reg2_sort_date_cb(GtkWidget *w, gpointer data)
{
    GNCSplitReg2 *gsr = data;
    gnc_split_reg2_sort(gsr, BY_DATE);
}

void
gnc_split_reg2_sort_date_entered_cb(GtkWidget *w, gpointer data)
{
    GNCSplitReg2 *gsr = data;
    gnc_split_reg2_sort(gsr, BY_DATE_ENTERED);
}

void
gnc_split_reg2_sort_date_reconciled_cb(GtkWidget *w, gpointer data)
{
    GNCSplitReg2 *gsr = data;
    gnc_split_reg2_sort(gsr, BY_DATE_RECONCILED);
}

void
gnc_split_reg2_sort_num_cb(GtkWidget *w, gpointer data)
{
    GNCSplitReg2 *gsr = data;
    gnc_split_reg2_sort(gsr, BY_NUM);
}

void
gnc_split_reg2_sort_amount_cb(GtkWidget *w, gpointer data)
{
    GNCSplitReg2 *gsr = data;
    gnc_split_reg2_sort(gsr, BY_AMOUNT);
}

void
gnc_split_reg2_sort_memo_cb(GtkWidget *w, gpointer data)
{
    GNCSplitReg2 *gsr = data;
    gnc_split_reg2_sort(gsr, BY_MEMO);
}

void
gnc_split_reg2_sort_desc_cb(GtkWidget *w, gpointer data)
{
    GNCSplitReg2 *gsr = data;
    gnc_split_reg2_sort(gsr, BY_DESC);
}

void
gnc_split_reg2_sort_action_cb(GtkWidget *w, gpointer data)
{
    GNCSplitReg2 *gsr = data;
    gnc_split_reg2_sort(gsr, BY_ACTION);
}

void
gnc_split_reg2_sort_notes_cb(GtkWidget *w, gpointer data)
{
    GNCSplitReg2 *gsr = data;
    gnc_split_reg2_sort(gsr, BY_NOTES);
}



void
gnc_split_reg2_handle_exchange_cb (GtkWidget *w, gpointer data)
{
/*FIXME*/
#ifdef skip
    GNCSplitReg2 *gsr = data;
    SplitRegister *reg = gnc_ledger_display2_get_split_register (gsr->ledger);

    /* XXX Ignore the return value -- we don't care if this succeeds */
    (void)gnc_split_register_handle_exchange (reg, TRUE);
#endif
}


static void
gnc_split_reg2_record (GNCSplitReg2 *gsr)
{
/*FIXME*/
#ifdef skip
    SplitRegister *reg;
    Transaction *trans;

    ENTER("gsr=%p", gsr);

    reg = gnc_ledger_display2_get_split_register (gsr->ledger);
    trans = gnc_split_register_get_current_trans (reg);

    if (!gnc_split_register_save (reg, TRUE))
    {
        LEAVE("no save");
        return;
    }

    gsr2_emit_include_date_signal( gsr, xaccTransGetDate(trans) );

    /* Explicit redraw shouldn't be needed,
     * since gui_refresh events should handle this. */
    /* gnc_split_register_redraw (reg); */
    LEAVE(" ");
#endif
}

static gboolean
gnc_split_reg2_match_trans_row( VirtualLocation virt_loc,
                               gpointer user_data )
{
/*FIXME*/
#ifdef skip
    GNCSplitReg2 *gsr = user_data;
    CursorClass cursor_class;
    SplitRegister *sr;

    sr = gnc_ledger_display2_get_split_register (gsr->ledger);
    cursor_class = gnc_split_register_get_cursor_class (sr, virt_loc.vcell_loc);

    return (cursor_class == CURSOR_CLASS_TRANS);
#endif
return FALSE;
}

static void
gnc_split_reg2_goto_next_trans_row (GNCSplitReg2 *gsr)
{
/*FIXME*/
#ifdef skip
    ENTER("gsr=%p", gsr);
    gnucash_register_goto_next_matching_row( gsr->reg,
            gnc_split_reg2_match_trans_row,
            gsr );
    LEAVE(" ");
#endif
}

void
gnc_split_reg2_enter (GNCSplitReg2 *gsr, gboolean next_transaction) //this works
{
    GncTreeViewSplitReg *view = gnc_ledger_display2_get_split_view_register (gsr->ledger);
    GncTreeModelSplitReg *model;
    gboolean goto_blank;

    ENTER("gsr=%p, next_transaction=%s", gsr, next_transaction ? "TRUE" : "FALSE");

    model = gnc_ledger_display2_get_split_model_register (gsr->ledger);

    goto_blank = gnc_gconf_get_bool (GCONF_GENERAL_REGISTER,
                                    "enter_moves_to_end", NULL);

    /* If we are in single or double line mode and we hit enter
     * on the blank split, go to the blank split instead of the
     * next row. This prevents the cursor from jumping around
     * when you are entering transactions. */
    if ( !goto_blank && !next_transaction )
    {
        SplitRegisterStyle2 style = model->style;

        if (style == REG2_STYLE_LEDGER)
        {
            Split *blank_split;

            blank_split = gnc_tree_view_split_reg_get_blank_split (view);
            if (blank_split != NULL)
            {
                Split *current_split;

                current_split = gnc_tree_view_split_reg_get_current_split (view);

                if (blank_split == current_split)
                    goto_blank = TRUE;
            }
        }
    }

    /* First record the transaction */
    if (gnc_tree_view_split_reg_enter (view))
    {
        if (!goto_blank && next_transaction)
            gnc_tree_view_split_reg_expand_current_trans (view, FALSE);

        /* Now move. */
        if (goto_blank)
            gnc_tree_view_split_reg_jump_to_blank (view);
        else if (next_transaction)
            gnc_tree_view_split_reg_goto_rel_trans_row (view, 1);
    }
    LEAVE(" ");
}

void
gsr2_default_enter_handler( GNCSplitReg2 *gsr, gpointer data )
{
/*FIXME    gnc_split_reg2_enter( gsr, FALSE );*/
}


#ifdef skip
void
gnc_split_reg2_record_cb (GnucashRegister *reg, gpointer data)
{
/*FIXME    gsr2_emit_simple_signal( (GNCSplitReg2*)data, "enter_ent" );*/
}
#endif


void
gnc_split_reg2_size_allocate (GtkWidget *widget,
                             GtkAllocation *allocation,
                             gpointer user_data)
{
    GNCSplitReg2 *gsr = user_data;
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
gsr2_create_summary_bar( GNCSplitReg2 *gsr )
{
    GtkWidget *summarybar;

    gsr->cleared_label    = NULL;
    gsr->balance_label    = NULL;
    gsr->reconciled_label = NULL;
    gsr->future_label     = NULL;
    gsr->projectedminimum_label  = NULL;
    gsr->shares_label     = NULL;
    gsr->value_label      = NULL;

    if ( gnc_ledger_display2_type(gsr->ledger) >= LD2_SUBACCOUNT )
    {
        gsr->summarybar = NULL;
        return NULL;
    }

    summarybar = gtk_hbox_new (FALSE, 4);

    if (!xaccAccountIsPriced(gnc_ledger_display2_leader(gsr->ledger)))
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
    gsr2_redraw_all_cb (NULL, gsr);
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
gnc_split_reg2_get_placeholder( GNCSplitReg2 *gsr )
{
    Account *leader;
    GncTreeModelSplitReg *model;
    gboolean single_account;

    if (gsr == NULL)
        return PLACEHOLDER_NONE;

    model = gnc_ledger_display2_get_split_model_register( gsr->ledger );

    switch (model->type)
    {
    case GENERAL_LEDGER2:
    case INCOME_LEDGER2:
    case PORTFOLIO_LEDGER2:
    case SEARCH_LEDGER2:
        single_account = FALSE;
        break;
    default:
        single_account = TRUE;
        break;
    }

    leader = gnc_ledger_display2_leader( gsr->ledger );

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
    GNCSplitReg2 *gsr;
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
gnc_split_reg2_determine_read_only( GNCSplitReg2 *gsr )
{
    dialog_args *args = g_malloc(sizeof(dialog_args));
    GncTreeViewSplitReg *view;

    if (qof_book_is_readonly (gnc_get_current_book()))
    {
        /* Is the book read-only? Then for sure also make this register
        read-only. */
        gsr->read_only = TRUE;
    }

    if ( !gsr->read_only )
    {

        switch (gnc_split_reg2_get_placeholder(gsr))
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
}

static
GtkWidget *
gnc_split_reg2_get_parent( GNCLedgerDisplay2 *ledger )
{
    GNCSplitReg2 *gsr =
        GNC_SPLIT_REG2(gnc_ledger_display2_get_user_data( ledger ));

    if (gsr == NULL)
        return NULL;

    return gsr->window;
}

static
void
gsr2_emit_help_changed (GncTreeViewSplitReg *view, gpointer user_data ) //this works
{
    gsr2_emit_simple_signal ((GNCSplitReg2*)user_data, "help-changed" );
}

static
void
gsr2_emit_include_date_signal( GNCSplitReg2 *gsr, time_t date )
{
    g_signal_emit_by_name( gsr, "include-date", date, NULL );
}

static
void
gsr2_emit_simple_signal( GNCSplitReg2 *gsr, const char *sigName ) //this works
{
    g_signal_emit_by_name( gsr, sigName, NULL );
}

GncTreeViewSplitReg *
gnc_split_reg2_get_register (GNCSplitReg2 *gsr )
{
    if ( !gsr )
        return NULL;

    return gnc_ledger_display2_get_split_view_register (gsr->ledger);
}

SortType
gnc_split_reg2_get_sort_type( GNCSplitReg2 *gsr )
{
    g_assert( gsr );
    return gsr->sort_type;
}

void
gnc_split_reg2_set_sort_type( GNCSplitReg2 *gsr, SortType t )
{
    gnc_split_reg2_sort( gsr, t );
}

GtkWidget*
gnc_split_reg2_get_summarybar( GNCSplitReg2 *gsr )
{
    if ( !gsr ) return NULL;
    return gsr->summarybar;
}

gboolean
gnc_split_reg2_get_read_only( GNCSplitReg2 *gsr )
{
    g_assert( gsr );
    return gsr->read_only;
}

void
gnc_split_reg2_set_moved_cb( GNCSplitReg2 *gsr, GFunc cb, gpointer cb_data ) //this works
{
g_print("gnc_split_reg2_set_moved_cb\n");
    gnc_tree_view_split_reg_moved_cb (gnc_ledger_display2_get_split_view_register(gsr->ledger), cb, cb_data);
}
