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

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <time.h>

#include "gnc-split-reg.h"

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
#include "gnc-prefs.h"
#include "gnc-gui-query.h"
#include "gnc-gnome-utils.h"
#include "gnc-ledger-display.h"
#include "gnc-pricedb.h"
#include "gnc-ui-util.h"
#include "gnc-ui.h"
#include "gnc-uri-utils.h"
#include "gnc-filepath-utils.h"
#include "gnc-warnings.h"
#include "gnucash-sheet.h"
#include "gnucash-register.h"
#include "table-allgui.h"

#include "dialog-utils.h"

// static QofLogModule log_module = GNC_MOD_SX;
static QofLogModule log_module = GNC_MOD_GUI;

/***** PROTOTYPES ***************************************************/
void gnc_split_reg_raise( GNCSplitReg *gsr );

static GtkWidget* add_summary_label( GtkWidget *summarybar, gboolean pack_start,
                                     const char *label_str, GtkWidget *extra );

static void gsr_summarybar_set_arrow_draw (GNCSplitReg *gsr);

static void gnc_split_reg_determine_read_only( GNCSplitReg *gsr );
static gboolean is_trans_readonly_and_warn (GtkWindow *parent, Transaction *trans);

static GNCPlaceholderType gnc_split_reg_get_placeholder( GNCSplitReg *gsr );
static GtkWidget *gnc_split_reg_get_parent( GNCLedgerDisplay *ledger );

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

static void gnc_split_reg_ld_destroy( GNCLedgerDisplay *ledger );

static Transaction* create_balancing_transaction(QofBook *book, Account *account,
        time64 statement_date, gnc_numeric balancing_amount);

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
void gsr_default_unvoid_txn_handler ( GNCSplitReg *w, gpointer ud );
void gsr_default_reverse_txn_handler ( GNCSplitReg *w, gpointer ud );
void gsr_default_associate_handler ( GNCSplitReg *w, gboolean uri_is_file );
void gsr_default_execassociated_handler ( GNCSplitReg *w, gpointer ud );

static void gsr_emit_simple_signal       ( GNCSplitReg *gsr, const char *sigName );
static void gsr_emit_help_changed        ( GnucashRegister *reg, gpointer user_data );
static void gsr_emit_show_popup_menu     ( GnucashRegister *reg, gpointer user_data );
static void gsr_emit_include_date_signal ( GNCSplitReg *gsr, time64 date );

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


void gnc_split_reg_size_allocate( GtkWidget *widget,
                                  GtkAllocation *allocation,
                                  gpointer user_data );


static void gnc_split_reg_class_init( GNCSplitRegClass *klass );
static void gnc_split_reg_init( GNCSplitReg *gsr );
static void gnc_split_reg_init2( GNCSplitReg *gsr );
void gnc_split_reg_dispose(GObject *obj);

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
            NULL,               /* base_init */
            NULL,               /* base_finalize */
            (GClassInitFunc)gnc_split_reg_class_init,
            NULL,               /* class_finalize */
            NULL,               /* class_data */
            sizeof(GNCSplitReg),        /* */
            0,                  /* n_preallocs */
            (GInstanceInitFunc)gnc_split_reg_init,
        };

        gnc_split_reg_type = g_type_register_static( GTK_TYPE_BOX,
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
    SHOW_POPUP_MENU_SIGNAL,
    INCLUDE_DATE_SIGNAL,
    LAST_SIGNAL
};

static guint gnc_split_reg_signals[LAST_SIGNAL] = { 0 };

static void
gnc_split_reg_class_init( GNCSplitRegClass *klass )
{
    int i;
    GObjectClass *object_class;
    static struct similar_signal_info
    {
        enum gnc_split_reg_signal_enum s;
        const char *signal_name;
        guint defaultOffset;
    } signals[] =
    {
        { ENTER_ENT_SIGNAL,       "enter_ent",       G_STRUCT_OFFSET( GNCSplitRegClass, enter_ent_cb ) },
        { CANCEL_ENT_SIGNAL,      "cancel_ent",      G_STRUCT_OFFSET( GNCSplitRegClass, cancel_ent_cb ) },
        { DELETE_ENT_SIGNAL,      "delete_ent",      G_STRUCT_OFFSET( GNCSplitRegClass, delete_ent_cb ) },
        { REINIT_ENT_SIGNAL,      "reinit_ent",      G_STRUCT_OFFSET( GNCSplitRegClass, reinit_ent_cb ) },
        { DUP_ENT_SIGNAL,         "dup_ent",         G_STRUCT_OFFSET( GNCSplitRegClass, dup_ent_cb ) },
        { SCHEDULE_ENT_SIGNAL,    "schedule_ent",    G_STRUCT_OFFSET( GNCSplitRegClass, schedule_ent_cb ) },
        { EXPAND_ENT_SIGNAL,      "expand_ent",      G_STRUCT_OFFSET( GNCSplitRegClass, expand_ent_cb ) },
        { BLANK_SIGNAL,           "blank",           G_STRUCT_OFFSET( GNCSplitRegClass, blank_cb ) },
        { JUMP_SIGNAL,            "jump",            G_STRUCT_OFFSET( GNCSplitRegClass, jump_cb ) },
        { CUT_SIGNAL,             "cut",             G_STRUCT_OFFSET( GNCSplitRegClass, cut_cb ) },
        { CUT_TXN_SIGNAL,         "cut_txn",         G_STRUCT_OFFSET( GNCSplitRegClass, cut_txn_cb ) },
        { COPY_SIGNAL,            "copy",            G_STRUCT_OFFSET( GNCSplitRegClass, copy_cb ) },
        { COPY_TXN_SIGNAL,        "copy_txn",        G_STRUCT_OFFSET( GNCSplitRegClass, copy_txn_cb ) },
        { PASTE_SIGNAL,           "paste",           G_STRUCT_OFFSET( GNCSplitRegClass, paste_cb ) },
        { PASTE_TXN_SIGNAL,       "paste_txn",       G_STRUCT_OFFSET( GNCSplitRegClass, paste_txn_cb ) },
        { VOID_TXN_SIGNAL,        "void_txn",        G_STRUCT_OFFSET( GNCSplitRegClass, void_txn_cb ) },
        { UNVOID_TXN_SIGNAL,      "unvoid_txn",      G_STRUCT_OFFSET( GNCSplitRegClass, unvoid_txn_cb ) },
        { REVERSE_TXN_SIGNAL,     "reverse_txn",     G_STRUCT_OFFSET( GNCSplitRegClass, reverse_txn_cb ) },
        { HELP_CHANGED_SIGNAL,    "help-changed",    G_STRUCT_OFFSET( GNCSplitRegClass, help_changed_cb ) },
        { SHOW_POPUP_MENU_SIGNAL, "show-popup-menu", G_STRUCT_OFFSET( GNCSplitRegClass, show_popup_menu_cb ) },
        { INCLUDE_DATE_SIGNAL,    "include-date",    G_STRUCT_OFFSET( GNCSplitRegClass, include_date_cb ) },
        { LAST_SIGNAL, NULL, 0 }
    };

    object_class = (GObjectClass*) klass;

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
                      g_cclosure_marshal_VOID__INT, /* time64 == int */
                      G_TYPE_NONE, 1, G_TYPE_INT );

    g_assert( i == LAST_SIGNAL );

    /* Setup the default handlers. */
    klass->enter_ent_cb    = gsr_default_enter_handler;
    klass->cancel_ent_cb   = gsr_default_cancel_handler;
    klass->delete_ent_cb   = gsr_default_delete_handler;
    klass->reinit_ent_cb   = gsr_default_reinit_handler;
    klass->dup_ent_cb      = gsr_default_dup_handler;
    klass->schedule_ent_cb = gsr_default_schedule_handler;
    klass->expand_ent_cb   = gsr_default_expand_handler;
    klass->blank_cb        = gsr_default_blank_handler;
    klass->jump_cb         = gsr_default_jump_handler;
    klass->cut_cb          = gsr_default_cut_handler;
    klass->cut_txn_cb      = gsr_default_cut_txn_handler;
    klass->copy_cb         = gsr_default_copy_handler;
    klass->copy_txn_cb     = gsr_default_copy_txn_handler;
    klass->paste_cb        = gsr_default_paste_handler;
    klass->paste_txn_cb    = gsr_default_paste_txn_handler;
    klass->void_txn_cb     = gsr_default_void_txn_handler;
    klass->unvoid_txn_cb   = gsr_default_unvoid_txn_handler;
    klass->reverse_txn_cb  = gsr_default_reverse_txn_handler;

    klass->help_changed_cb = NULL;
    klass->show_popup_menu_cb = NULL;
    klass->include_date_cb = NULL;

    object_class->dispose = gnc_split_reg_dispose;
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
    gtk_orientable_set_orientation (GTK_ORIENTABLE(gsr), GTK_ORIENTATION_VERTICAL);

    gsr->sort_type = BY_STANDARD;
    gsr->sort_rev = FALSE;
    gsr->sort_arrow_handler_id = 0;
    gsr->filter_text = NULL;
    gsr->width = -1;
    gsr->height = -1;
    gsr->numRows = 10;
    gsr->read_only = FALSE;
}

static void
gnc_split_reg_pref_acc_labels (gpointer prefs, gchar *pref, gpointer user_data)
{
    GNCSplitReg *gsr = user_data;
    gnucash_register_refresh_from_prefs (gsr->reg);
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

    gnc_prefs_register_cb (GNC_PREFS_GROUP_GENERAL,
                           GNC_PREF_ACCOUNTING_LABELS,
                           gnc_split_reg_pref_acc_labels,
                           gsr);
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

    LEAVE(" ");
}

static
void
gsr_create_table( GNCSplitReg *gsr )
{
    GtkWidget *register_widget = NULL;
    SplitRegister *sr = NULL;

    Account * account = gnc_ledger_display_leader(gsr->ledger);
    const GncGUID * guid = xaccAccountGetGUID(account);
    gchar guidstr[GUID_ENCODING_LENGTH+1];
    gchar *state_section = NULL;
    guid_to_string_buff(guid, guidstr);
    state_section = g_strconcat (STATE_SECTION_REG_PREFIX, " ", guidstr, NULL);

    ENTER("gsr=%p", gsr);

    gnc_ledger_display_set_user_data( gsr->ledger, (gpointer)gsr );
    gnc_ledger_display_set_handlers( gsr->ledger,
                                     gnc_split_reg_ld_destroy,
                                     gnc_split_reg_get_parent );

    /* FIXME: We'd really rather pass this down... */
    sr = gnc_ledger_display_get_split_register( gsr->ledger );
    register_widget = gnucash_register_new( sr->table, state_section );
    gsr->reg = GNUCASH_REGISTER( register_widget );
    g_free (state_section);
    gtk_box_pack_start (GTK_BOX (gsr), GTK_WIDGET(gsr->reg), TRUE, TRUE, 0);
    gnucash_sheet_set_window (gnucash_register_get_sheet (gsr->reg), gsr->window);
    gtk_widget_show ( GTK_WIDGET(gsr->reg) );
    g_signal_connect (gsr->reg, "activate_cursor",
                      G_CALLBACK(gnc_split_reg_record_cb), gsr);
    g_signal_connect (gsr->reg, "redraw_all",
                      G_CALLBACK(gsr_redraw_all_cb), gsr);
    g_signal_connect (gsr->reg, "redraw_help",
                      G_CALLBACK(gsr_emit_help_changed), gsr);
    g_signal_connect (gsr->reg, "show_popup_menu",
                      G_CALLBACK(gsr_emit_show_popup_menu), gsr);

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
gnc_split_reg_dispose(GObject *obj)
{
    GNCSplitReg *gsr = GNC_SPLIT_REG(obj);

    if (gsr->filter_text)
        g_free (gsr->filter_text);
    gsr->filter_text = NULL;

    gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_GENERAL,
                                 GNC_PREF_ACCOUNTING_LABELS,
                                 gnc_split_reg_pref_acc_labels,
                                 gsr);

    if (gsr->reg)
    {
        g_signal_handlers_disconnect_by_data (gsr->reg, gsr);
        gtk_widget_destroy (GTK_WIDGET (gsr->reg));
    }
    gsr->reg = NULL;
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

static
void
gsr_redraw_all_cb (GnucashRegister *g_reg, gpointer data)
{
    GNCSplitReg *gsr = data;
    gnc_commodity * commodity;
    GNCPrintAmountInfo print_info;
    gnc_numeric amount;
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

    if (gsr->balance_label != NULL) // only test the first as they are a group
    {
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
    }

    // Sort label
    if (gsr->sort_label != NULL)
    {
        gchar *old_tt_text = gtk_widget_get_tooltip_text (GTK_WIDGET(gsr->sort_label));
        gchar *new_tt_text;
        gchar *text = NULL;

        switch (gsr->sort_type)
        {
        case (0):
            text =  _("None");
            break;
        case (1):
            text = _("Standard Order");
            break;
        case (2):
            text = _("Date");
            break;
        case (3):
            text = _("Date of Entry");
            break;
        case (4):
            text = _("Statement Date");
            break;
        case (5):
            text = _("Number");
            break;
        case (6):
            text = _("Amount");
            break;
        case (7):
            text = _("Memo");
            break;
        case (8):
            text = _("Description");
            break;
        case (9):
            text = _("Action");
            break;
        case (10):
            text = _("Notes");
            break;
        }

        if (gsr->sort_rev)
            gtk_widget_set_tooltip_text (GTK_WIDGET(gsr->sort_label), _("Descending"));
        else
            gtk_widget_set_tooltip_text (GTK_WIDGET(gsr->sort_label), _("Ascending"));

        new_tt_text = gtk_widget_get_tooltip_text (GTK_WIDGET(gsr->sort_label));

        // does the arrow need changing
        if (g_strcmp0 (old_tt_text, new_tt_text) != 0)
            gsr_summarybar_set_arrow_draw (gsr);

        if (old_tt_text)
            g_free (old_tt_text);

        if (new_tt_text)
            g_free (new_tt_text);

        gtk_label_set_text (GTK_LABEL(gsr->sort_label), text);
    }

    // Filter label
    if (gsr->filter_label != NULL)
    {
        gchar *old_tt_text = gtk_widget_get_tooltip_text (GTK_WIDGET(gsr->filter_label));

        // check for a change in text
        if (g_strcmp0 (old_tt_text, gsr->filter_text) != 0)
        {
            if (gsr->filter_text != NULL)
                gtk_label_set_text (GTK_LABEL(gsr->filter_label), _("Filtered"));
            else
                gtk_label_set_text (GTK_LABEL(gsr->filter_label), "");

            gtk_widget_set_tooltip_text (GTK_WIDGET(gsr->filter_label), gsr->filter_text);

            if (old_tt_text)
                g_free (old_tt_text);
        }
    }

    if (gsr->shares_label == NULL && gsr->value_label == NULL)
        return;
    amount = xaccAccountGetBalance( leader );
    if (reverse)
        amount = gnc_numeric_neg( amount );

   /* Print the summary share amount */
    if (gsr->shares_label != NULL)
    {
        char string[256];
        print_info = gnc_account_print_info( leader, TRUE );
        xaccSPrintAmount( string, amount, print_info );
        gnc_set_label_color( gsr->shares_label, amount );
        gtk_label_set_text( GTK_LABEL(gsr->shares_label), string );
    }

    /* Print the summary share value */
    if (gsr->value_label != NULL)
    {
        char string[256];
        QofBook *book = gnc_account_get_book (leader);
        GNCPriceDB *pricedb = gnc_pricedb_get_db (book);
        gnc_commodity *currency = gnc_default_currency ();
        gnc_numeric value =
            gnc_pricedb_convert_balance_latest_price (pricedb, amount,
                                                      commodity, currency);
        print_info = gnc_commodity_print_info (currency, TRUE);
        xaccSPrintAmount (string, value, print_info);
        gnc_set_label_color (gsr->value_label, amount);
        gtk_label_set_text (GTK_LABEL (gsr->value_label), string);

    }
}

static void
gnc_split_reg_ld_destroy( GNCLedgerDisplay *ledger )
{
    GNCSplitReg *gsr = gnc_ledger_display_get_user_data( ledger );

    Account * account = gnc_ledger_display_leader(ledger);
    const GncGUID * guid = xaccAccountGetGUID(account);
    gchar guidstr[GUID_ENCODING_LENGTH+1];
    gchar *state_section;
    gchar *acct_fullname;
    guid_to_string_buff(guid, guidstr);

    state_section = g_strconcat (STATE_SECTION_REG_PREFIX, " ", guidstr, NULL);

    if (g_strcmp0(guidstr, "00000000000000000000000000000000") == 0)
        acct_fullname = g_strdup(_("General Journal"));
    else
        acct_fullname = gnc_account_get_full_name(account);

    if (gsr)
    {
        SplitRegister *reg;

        reg = gnc_ledger_display_get_split_register (ledger);

        if (reg && reg->table)
            gnc_table_save_state (reg->table, state_section, acct_fullname);

        /*
         * Don't destroy the window here any more.  The register no longer
         * owns it.
         */
    }
    g_free (state_section);
    g_free (acct_fullname);

    gnc_ledger_display_set_user_data (ledger, NULL);
    g_object_unref (gsr);
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
gsr_default_cut_txn_handler (GNCSplitReg *gsr, gpointer data)
{
    CursorClass cursor_class;
    SplitRegister *reg;
    Transaction *trans;
    Split *split;
    GtkWidget *dialog;
    gint response;
    const gchar *warning;

    reg = gnc_ledger_display_get_split_register (gsr->ledger);

    /* get the current split based on cursor position */
    split = gnc_split_register_get_current_split (reg);
    if (split == NULL)
    {
        gnc_split_register_cancel_cursor_split_changes (reg);
        return;
    }

    trans = xaccSplitGetParent (split);
    cursor_class = gnc_split_register_get_current_cursor_class (reg);

    /* test for blank_split reference pointing to split */
    if (gnc_split_register_is_blank_split (reg, split))
        gnc_split_register_change_blank_split_ref (reg, split);

    /* Cutting the blank split just cancels */
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

    /* this is probably not required but leave as a double check */
    if (is_trans_readonly_and_warn (GTK_WINDOW(gsr->window), trans))
        return;

    /* On a split cursor, just delete the one split. */
    if (cursor_class == CURSOR_CLASS_SPLIT)
    {
        const char *format = _("Cut the split '%s' from the transaction '%s'?");
        const char *recn_warn = _("You would be removing a reconciled split! "
                                  "This is not a good idea as it will cause your "
                                  "reconciled balance to be off.");
        const char *anchor_error = _("You cannot cut this split.");
        const char *anchor_split = _("This is the split anchoring this transaction "
                                     "to the register. You may not remove it from "
                                     "this register window. You may remove the "
                                     "entire transaction from this window, or you "
                                     "may navigate to a register that shows "
                                     "another side of this same transaction and "
                                     "remove the split from that register.");
        char *buf = NULL;
        const char *memo;
        const char *desc;
        char recn;

        if (reg->type != GENERAL_JOURNAL) // no anchoring split
        {
            if (split == gnc_split_register_get_current_trans_split (reg, NULL))
            {
                dialog = gtk_message_dialog_new (GTK_WINDOW(gsr->window),
                                                 GTK_DIALOG_MODAL
                                                 | GTK_DIALOG_DESTROY_WITH_PARENT,
                                                 GTK_MESSAGE_ERROR,
                                                 GTK_BUTTONS_OK,
                                                 "%s", anchor_error);
                gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG(dialog),
                         "%s", anchor_split);
                gtk_dialog_run (GTK_DIALOG(dialog));
                gtk_widget_destroy (dialog);
                return;
            }
        }
        memo = xaccSplitGetMemo (split);
        memo = (memo && *memo) ? memo : _("(no memo)");

        desc = xaccTransGetDescription (trans);
        desc = (desc && *desc) ? desc : _("(no description)");

        /* ask for user confirmation before performing permanent damage */
        buf = g_strdup_printf (format, memo, desc);
        dialog = gtk_message_dialog_new (GTK_WINDOW(gsr->window),
                                         GTK_DIALOG_MODAL
                                         | GTK_DIALOG_DESTROY_WITH_PARENT,
                                         GTK_MESSAGE_QUESTION,
                                         GTK_BUTTONS_NONE,
                                         "%s", buf);
        g_free (buf);
        recn = xaccSplitGetReconcile (split);
        if (recn == YREC || recn == FREC)
        {
            gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG(dialog),
                    "%s", recn_warn);
            warning = GNC_PREF_WARN_REG_SPLIT_DEL_RECD;
        }
        else
        {
            warning = GNC_PREF_WARN_REG_SPLIT_DEL;
        }

        gtk_dialog_add_button (GTK_DIALOG(dialog),
                               _("_Cancel"), GTK_RESPONSE_CANCEL);
        gnc_gtk_dialog_add_button (dialog, _("_Cut Split"),
                                   "edit-delete", GTK_RESPONSE_ACCEPT);
        response = gnc_dialog_run (GTK_DIALOG(dialog), warning);
        gtk_widget_destroy (dialog);
        if (response != GTK_RESPONSE_ACCEPT)
            return;

        gnc_split_register_cut_current (reg);
        return;
    }

    /* On a transaction cursor with 2 or fewer splits in single or double
     * mode, we just delete the whole transaction, kerblooie */
    {
        const char *title = _("Cut the current transaction?");
        const char *recn_warn = _("You would be removing a transaction "
                                  "with reconciled splits! "
                                  "This is not a good idea as it will cause your "
                                  "reconciled balance to be off.");

        dialog = gtk_message_dialog_new (GTK_WINDOW(gsr->window),
                                         GTK_DIALOG_MODAL
                                         | GTK_DIALOG_DESTROY_WITH_PARENT,
                                         GTK_MESSAGE_WARNING,
                                         GTK_BUTTONS_NONE,
                                         "%s", title);
        if (xaccTransHasReconciledSplits (trans))
        {
            gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG(dialog),
                     "%s", recn_warn);
            warning = GNC_PREF_WARN_REG_TRANS_DEL_RECD;
        }
        else
        {
            warning = GNC_PREF_WARN_REG_TRANS_DEL;
        }
        gtk_dialog_add_button (GTK_DIALOG(dialog),
                               _("_Cancel"), GTK_RESPONSE_CANCEL);
        gnc_gtk_dialog_add_button (dialog, _("_Cut Transaction"),
                                  "edit-delete", GTK_RESPONSE_ACCEPT);
        response =  gnc_dialog_run (GTK_DIALOG(dialog), warning);
        gtk_widget_destroy (dialog);
        if (response != GTK_RESPONSE_ACCEPT)
            return;

        gnc_split_register_cut_current (reg);
        return;
    }
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
        gnc_error_dialog (GTK_WINDOW (gsr->window), "%s",
                          _("A reversing entry has already been created for this transaction."));
        return;
    }

    new_trans = xaccTransReverse(trans);

    /* Clear transaction level info */
    xaccTransSetDatePostedSecsNormalized(new_trans, gnc_time (NULL));
    xaccTransSetDateEnteredSecs(new_trans, gnc_time (NULL));

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
is_trans_readonly_and_warn (GtkWindow *parent, Transaction *trans)
{
    GtkWidget *dialog;
    const gchar *reason;
    const gchar *title = _("Cannot modify or delete this transaction.");
    const gchar *message =
        _("This transaction is marked read-only with the comment: '%s'");

    if (!trans) return FALSE;

    if (xaccTransIsReadonlyByPostedDate (trans))
    {
        dialog = gtk_message_dialog_new(parent,
                                        0,
                                        GTK_MESSAGE_ERROR,
                                        GTK_BUTTONS_OK,
                                        "%s", title);
        gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(dialog),
                "%s", _("The date of this transaction is older than the \"Read-Only Threshold\" set for this book. "
                        "This setting can be changed in File -> Properties -> Accounts."));
        gtk_dialog_run(GTK_DIALOG(dialog));
        gtk_widget_destroy(dialog);
        return TRUE;
    }

    reason = xaccTransGetReadOnly (trans);
    if (reason)
    {
        dialog = gtk_message_dialog_new(parent,
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
    if (is_trans_readonly_and_warn(GTK_WINDOW(gsr->window), trans))
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
        warning = GNC_PREF_WARN_REG_SPLIT_DEL_ALL_RECD;
    }
    else
    {
        warning = GNC_PREF_WARN_REG_SPLIT_DEL_ALL;
    }

    gtk_dialog_add_button(GTK_DIALOG(dialog),
                          _("_Cancel"), GTK_RESPONSE_CANCEL);
    gnc_gtk_dialog_add_button(dialog,
                              /* Translators: This is the confirmation button in a warning dialog */
                              _("_Remove Splits"),
                              "edit-delete", GTK_RESPONSE_ACCEPT);
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

static void
gsr_default_associate_handler_file (GNCSplitReg *gsr, Transaction *trans, gboolean have_uri)
{
    GtkWidget *dialog;
    gint       response;
    gboolean   path_head_set = FALSE;
    gchar     *path_head = gnc_prefs_get_string (GNC_PREFS_GROUP_GENERAL, "assoc-head");

    dialog = gtk_file_chooser_dialog_new (_("Associate File with Transaction"),
                                     GTK_WINDOW(gsr->window),
                                     GTK_FILE_CHOOSER_ACTION_OPEN,
                                     _("_Remove"), GTK_RESPONSE_REJECT,
                                     _("_Cancel"), GTK_RESPONSE_CANCEL,
                                     _("_OK"), GTK_RESPONSE_ACCEPT,
                                     NULL);

    gtk_file_chooser_set_local_only (GTK_FILE_CHOOSER(dialog), FALSE);

    path_head_set = (path_head && *path_head != '\0'); // not default entry

    if (have_uri)
    {
        gchar *file_uri = NULL;
        const gchar *uri = xaccTransGetAssociation (trans);
        gchar *scheme = gnc_uri_get_scheme (uri);

        if (!scheme) // relative path
        {
            gchar *file_path = NULL;
            if (path_head_set) // not default entry
                file_path = gnc_file_path_absolute (gnc_uri_get_path (path_head), uri);
            else
                file_path = gnc_file_path_absolute (NULL, uri);

            file_uri = gnc_uri_create_uri ("file", NULL, 0, NULL, NULL, file_path);
            g_free (file_path);
        }

        if (g_strcmp0 (scheme, "file") == 0) // absolute path
            file_uri = g_strdup (uri);

        if (file_uri)
        {
            GtkWidget *label;
            gchar *file_uri_u = g_uri_unescape_string (file_uri, NULL);
            gchar *filename = gnc_uri_get_path (file_uri_u);
            gchar *uri_label;

#ifdef G_OS_WIN32 // make path look like a traditional windows path
            filename = g_strdelimit (filename, "/", '\\');
#endif
            uri_label = g_strconcat (_("Existing Association is '"), filename, "'", NULL);

            PINFO("Path head: '%s', URI: '%s', Filename: '%s'", path_head, uri, filename);
            label = gtk_label_new (uri_label);
            gtk_file_chooser_set_extra_widget (GTK_FILE_CHOOSER(dialog), label);
            gtk_label_set_ellipsize (GTK_LABEL(label), PANGO_ELLIPSIZE_START);

            // Set the style context for this label so it can be easily manipulated with css
            gnc_widget_style_context_add_class (GTK_WIDGET(label), "gnc-class-highlight");
            gtk_file_chooser_set_uri (GTK_FILE_CHOOSER(dialog), file_uri);

            g_free (uri_label);
            g_free (filename);
            g_free (file_uri_u);
            g_free (file_uri);
        }
    }
    response = gtk_dialog_run (GTK_DIALOG (dialog));

    if (response == GTK_RESPONSE_REJECT)
        xaccTransSetAssociation (trans, "");

    if (response == GTK_RESPONSE_ACCEPT)
    {
        gchar *dialog_uri = gtk_file_chooser_get_uri (GTK_FILE_CHOOSER (dialog));

        // prior to 3.5, assoc-head could be with or without a trailing '/'
        if (path_head_set && !g_str_has_suffix (path_head, "/"))
        {
            gchar *folder_with_slash = g_strconcat (path_head, "/", NULL);
            g_free (path_head);
            path_head = g_strdup (folder_with_slash);
            g_free (folder_with_slash);

            if (!gnc_prefs_set_string (GNC_PREFS_GROUP_GENERAL, "assoc-head", path_head))
                PINFO("Failed to save preference at %s, %s with %s",
                       GNC_PREFS_GROUP_GENERAL, "assoc-head", path_head);
        }

        PINFO("Dialog File URI: '%s', Path head: '%s'", dialog_uri, path_head);

        // relative paths do not start with a '/'
        if (path_head_set && g_str_has_prefix (dialog_uri, path_head))
        {
            const gchar *part = dialog_uri + strlen (path_head);

            PINFO("Dialog URI: '%s', Part: '%s'", dialog_uri, part);
            xaccTransSetAssociation (trans, part);
        }
        else
        {
            PINFO("Dialog URI: '%s'", dialog_uri);
            xaccTransSetAssociation (trans, dialog_uri);
        }
        g_free (dialog_uri);
    }

    gtk_widget_destroy (dialog);
}

static void
gsr_default_associate_handler_location_ok_cb (GtkEditable *editable, gpointer user_data)
{
    GtkWidget *ok_button = user_data;
    gboolean have_scheme = FALSE;
    gchar *text = gtk_editable_get_chars (editable, 0, -1);
    gchar *scheme;

    if (text && *text != '\0')
    {
        scheme = gnc_uri_get_scheme (text);
        if (scheme)
            have_scheme = TRUE;
        g_free (scheme);
    }
    gtk_widget_set_sensitive (ok_button, have_scheme);
    g_free (text);
}

static void
gsr_default_associate_handler_location (GNCSplitReg *gsr, Transaction *trans, gboolean have_uri)
{
    GtkWidget *dialog, *entry, *label, *content_area, *ok_button;
    gint response;

    dialog = gtk_dialog_new_with_buttons (_("Associate Location with Transaction"),
                                     GTK_WINDOW(gsr->window),
                                     GTK_DIALOG_MODAL,
                                     _("_Remove"), GTK_RESPONSE_REJECT,
                                     _("_Cancel"), GTK_RESPONSE_CANCEL,
                                     // OK Button added below
                                     NULL);

    ok_button = gtk_dialog_add_button (GTK_DIALOG(dialog), _("_OK"), GTK_RESPONSE_ACCEPT);
    gtk_widget_set_sensitive (ok_button, FALSE);

    content_area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));

    // add the entry text
    entry = gtk_entry_new ();
    gtk_entry_set_width_chars (GTK_ENTRY (entry), 80);
    gtk_entry_set_activates_default (GTK_ENTRY (entry), TRUE);

    g_signal_connect (entry, "changed",
        G_CALLBACK(gsr_default_associate_handler_location_ok_cb), ok_button);

    // add a label and set entry text if required
    if (have_uri)
    {
        label = gtk_label_new (_("Amend URL"));
        gtk_entry_set_text (GTK_ENTRY (entry), xaccTransGetAssociation (trans));
    }
    else
        label = gtk_label_new (_("Enter URL like https://www.gnucash.org"));

    // pack label and entry to content area
    gnc_label_set_alignment (label, 0.0, 0.5);
    gtk_container_add (GTK_CONTAINER (content_area), label);
    gtk_container_add (GTK_CONTAINER (content_area), entry);

    // set spacings
    gtk_container_set_border_width (GTK_CONTAINER (dialog), 12);

    // set the default response
    gtk_dialog_set_default_response (GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT);

    gtk_widget_show_all (dialog);

    // run the dialog
    response = gtk_dialog_run (GTK_DIALOG (dialog));

    if (response == GTK_RESPONSE_REJECT)
        xaccTransSetAssociation (trans, "");

    if (response == GTK_RESPONSE_ACCEPT)
    {
        const gchar *dialog_uri = gtk_entry_get_text (GTK_ENTRY (entry));

        DEBUG("Location URI: %s\n", dialog_uri);
        xaccTransSetAssociation (trans, dialog_uri);
    }
    gtk_widget_destroy (dialog);
}

static gchar*
gsr_convert_associate_uri (Transaction *trans)
{
    const gchar *uri = xaccTransGetAssociation (trans); // get the existing uri
    const gchar *part = NULL;

    if (!uri)
        return NULL;

    if (g_str_has_prefix (uri, "file:") && !g_str_has_prefix (uri,"file://"))
    {
        // fix an error when storing relative paths in version earlier than 3.5
        // relative paths are stored without a leading "/" and in native form
        if (g_str_has_prefix (uri,"file:/") && !g_str_has_prefix (uri,"file://"))
            part = uri + strlen ("file:/");
        else if (g_str_has_prefix (uri,"file:") && !g_str_has_prefix (uri,"file://"))
            part = uri + strlen ("file:");

        if (part)
        {
            xaccTransSetAssociation (trans, part);
            return g_strdup (part);
        }
    }
    return g_strdup (uri);
}

/**
 * Associates a URI with the current transaction.
 **/
void
gsr_default_associate_handler (GNCSplitReg *gsr, gboolean uri_is_file)
{
    SplitRegister *reg = gnc_ledger_display_get_split_register (gsr->ledger);
    Split *split = gnc_split_register_get_current_split (reg);
    Transaction *trans;
    CursorClass cursor_class;
    const gchar *uri;
    gboolean have_uri = FALSE;

    /* get the current split based on cursor position */
    if (!split)
    {
        gnc_split_register_cancel_cursor_split_changes (reg);
        return;
    }

    trans = xaccSplitGetParent (split);
    cursor_class = gnc_split_register_get_current_cursor_class (reg);

    if (cursor_class == CURSOR_CLASS_NONE)
        return;

    // fix an earlier error when storing relative paths in version 3.3
    uri = gsr_convert_associate_uri (trans);

    if (is_trans_readonly_and_warn (GTK_WINDOW(gsr->window), trans))
        return;

    // Check for uri is empty or NULL
    if (uri && *uri != '\0')
    {
        gchar *scheme = gnc_uri_get_scheme (uri);
        have_uri = TRUE;

        if (!scheme || g_strcmp0 (scheme, "file") == 0) // use the correct dialog
            uri_is_file = TRUE;
        else
            uri_is_file = FALSE;

        g_free (scheme);
    }

    if (uri_is_file == TRUE)
        gsr_default_associate_handler_file (gsr, trans, have_uri);
    else
        gsr_default_associate_handler_location (gsr, trans, have_uri);
}

/**
 * Executes the associated link with the current transaction.
 **/
void
gsr_default_execassociated_handler (GNCSplitReg *gsr, gpointer data)
{
    CursorClass cursor_class;
    SplitRegister *reg = gnc_ledger_display_get_split_register (gsr->ledger);
    Transaction *trans;
    Split *split = gnc_split_register_get_current_split (reg);
    const char *uri;
    const char *run_uri = NULL;
    gchar *uri_scheme;

    /* get the current split based on cursor position */
    if (!split)
    {
        gnc_split_register_cancel_cursor_split_changes (reg);
        return;
    }

    trans = xaccSplitGetParent (split);
    cursor_class = gnc_split_register_get_current_cursor_class (reg);

    if (cursor_class == CURSOR_CLASS_NONE)
        return;

#ifdef DUMP_FUNCTIONS
    if (qof_log_check (log_module, QOF_LOG_DEBUG))
        xaccTransDump (trans, "ExecAssociated");
#endif

    // fix an earlier error when storing relative paths in version 3.3
    uri = gsr_convert_associate_uri (trans);

    if (!uri && g_strcmp0 (uri, "") == 0)
        gnc_error_dialog (GTK_WINDOW (gsr->window), "%s", _("This transaction is not associated with a URI."));
    else
    {
        gchar *scheme = gnc_uri_get_scheme (uri);

        if (!scheme) // relative path
        {
            gchar *path_head = gnc_prefs_get_string (GNC_PREFS_GROUP_GENERAL, "assoc-head");
            gchar *file_path;

            if (path_head && g_strcmp0 (path_head, "") != 0) // not default entry
                file_path = gnc_file_path_absolute (gnc_uri_get_path (path_head), uri);
            else
                file_path = gnc_file_path_absolute (NULL, uri);

            run_uri = gnc_uri_create_uri ("file", NULL, 0, NULL, NULL, file_path);
            g_free (path_head);
            g_free (file_path);
        }

        if (!run_uri)
            run_uri = g_strdup (uri);

        uri_scheme = gnc_uri_get_scheme (run_uri);

        if (uri_scheme) // make sure we have a scheme entry
        {
            gnc_launch_assoc (GTK_WINDOW (gsr->window), run_uri);
            g_free (uri_scheme);
        }
        else
            gnc_error_dialog (GTK_WINDOW (gsr->window), "%s", _("This transaction is not associated with a valid URI."));
    }
    return;
}

void
gsr_default_delete_handler( GNCSplitReg *gsr, gpointer data )
{
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
    cursor_class = gnc_split_register_get_current_cursor_class (reg);

    /* test for blank_split reference pointing to split */
    if (gnc_split_register_is_blank_split (reg, split))
        gnc_split_register_change_blank_split_ref (reg, split);

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

    if (is_trans_readonly_and_warn(GTK_WINDOW(gsr->window), trans))
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
                                     "this register window. You may delete the "
                                     "entire transaction from this window, or you "
                                     "may navigate to a register that shows "
                                     "another side of this same transaction and "
                                     "delete the split from that register.");
        char *buf = NULL;
        const char *memo;
        const char *desc;
        char recn;

        if (reg->type != GENERAL_JOURNAL) // no anchoring split
        {
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
            warning = GNC_PREF_WARN_REG_SPLIT_DEL_RECD;
        }
        else
        {
            warning = GNC_PREF_WARN_REG_SPLIT_DEL;
        }

        gtk_dialog_add_button(GTK_DIALOG(dialog),
                              _("_Cancel"), GTK_RESPONSE_CANCEL);
        gnc_gtk_dialog_add_button(dialog, _("_Delete Split"),
                                  "edit-delete", GTK_RESPONSE_ACCEPT);
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
            warning = GNC_PREF_WARN_REG_TRANS_DEL_RECD;
        }
        else
        {
            warning = GNC_PREF_WARN_REG_TRANS_DEL;
        }
        gtk_dialog_add_button(GTK_DIALOG(dialog),
                              _("_Cancel"), GTK_RESPONSE_CANCEL);
        gnc_gtk_dialog_add_button(dialog, _("_Delete Transaction"),
                                  "edit-delete", GTK_RESPONSE_ACCEPT);
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
    GncGUID *fromSXId = NULL;
    SchedXaction *theSX = NULL;
    GList *sxElts;
    SplitRegister *reg = gnc_ledger_display_get_split_register( gsr->ledger );
    Transaction *pending_trans = gnc_split_register_get_current_trans (reg);

    /* If the transaction has a sched-xact KVP frame, then go to the editor
     * for the existing SX; otherwise, do the sx-from-trans dialog. */

    qof_instance_get (QOF_INSTANCE (pending_trans),
              "from-sched-xaction", &fromSXId,
              NULL);

    /* Get the correct SX */
    for ( sxElts = gnc_book_get_schedxactions (gnc_get_current_book())->sx_list;
          (!theSX) && sxElts;
          sxElts = sxElts->next )
    {
        SchedXaction *sx = (SchedXaction*)sxElts->data;
        theSX =
        ((guid_equal (xaccSchedXactionGetGUID (sx), fromSXId))
         ? sx : NULL);
    }
    guid_free (fromSXId);

    if ( theSX )
    {
        gnc_ui_scheduled_xaction_editor_dialog_create(GTK_WINDOW(data), theSX, FALSE);
        return;
    }
    gnc_sx_create_from_trans(GTK_WINDOW(data), pending_trans);
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
        ( ( gtk_check_menu_item_get_active (GTK_CHECK_MENU_ITEM(gsr->split_menu_check)) ? 1 : -1 )
          + ( gtk_check_menu_item_get_active (GTK_CHECK_MENU_ITEM(gsr->split_popup_check)) ? 1 : -1 )
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
gnc_split_reg_focus_on_sheet (GNCSplitReg *gsr)
{
    GnucashRegister *reg = gsr->reg;
    GnucashSheet *sheet = gnucash_register_get_sheet (reg);

    // Make sure the sheet is the focus
    if (!gtk_widget_has_focus(GTK_WIDGET (sheet)))
        gtk_widget_grab_focus (GTK_WIDGET (sheet));
}

void
gnc_split_reg_balancing_entry(GNCSplitReg *gsr, Account *account,
                              time64 statement_date, gnc_numeric balancing_amount)
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
                             time64 statement_date, gnc_numeric balancing_amount)
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
    xaccTransSetCurrency(trans, gnc_account_or_default_currency(account, NULL));
    xaccTransSetDatePostedSecsNormalized(trans, statement_date);
    xaccTransSetDescription(trans, _("Balancing entry from reconciliation"));
    /* We also must set a new DateEntered on the new entry
     * because otherwise the ordering is not deterministic */
    xaccTransSetDateEnteredSecs(trans, gnc_time(NULL));

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
gnc_split_reg_change_style (GNCSplitReg *gsr, SplitRegisterStyle style, gboolean refresh)
{
    SplitRegister *reg = gnc_ledger_display_get_split_register (gsr->ledger);

    if (style == reg->style)
        return;

    gnc_split_register_config (reg, reg->type, style, reg->use_double_line);
    if (refresh)
        gnc_ledger_display_refresh (gsr->ledger);
}

void
gnc_split_reg_style_ledger_cb (GtkWidget *w, gpointer data)
{
    GNCSplitReg *gsr = data;

    if (!gtk_check_menu_item_get_active (GTK_CHECK_MENU_ITEM(w)))
        return;

    gnc_split_reg_change_style (gsr, REG_STYLE_LEDGER, TRUE);
}

void
gnc_split_reg_style_auto_ledger_cb (GtkWidget *w, gpointer data)
{
    GNCSplitReg *gsr = data;

    if (!gtk_check_menu_item_get_active (GTK_CHECK_MENU_ITEM(w)))
        return;

    gnc_split_reg_change_style (gsr, REG_STYLE_AUTO_LEDGER, TRUE);
}

void
gnc_split_reg_style_journal_cb (GtkWidget *w, gpointer data)
{
    GNCSplitReg *gsr = data;

    if (!gtk_check_menu_item_get_active (GTK_CHECK_MENU_ITEM(w)))
        return;

    gnc_split_reg_change_style (gsr, REG_STYLE_JOURNAL, TRUE);
}

void
gnc_split_reg_double_line_cb (GtkWidget *w, gpointer data)
{
    GNCSplitReg *gsr = data;
    SplitRegister *reg = gnc_ledger_display_get_split_register (gsr->ledger);
    gboolean use_double_line;

    use_double_line = gtk_check_menu_item_get_active (GTK_CHECK_MENU_ITEM(w));
    if ( use_double_line == reg->use_double_line )
        return;

    gnc_split_register_config( reg, reg->type, reg->style, use_double_line );
    gnc_ledger_display_refresh( gsr->ledger );
}

static void
gnc_split_reg_sort_force( GNCSplitReg *gsr, SortType sort_code, gboolean force )
{
    Query *query = gnc_ledger_display_get_query( gsr->ledger );
    gboolean show_present_divider = FALSE;
    GSList *p1 = NULL, *p2 = NULL, *p3 = NULL, *standard;
    SplitRegister *reg;

    if ((gsr->sort_type == sort_code) && !force)
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
        break;
    }

    qof_query_set_sort_order( query, p1, p2, p3 );
    reg = gnc_ledger_display_get_split_register( gsr->ledger );
    gnc_split_register_show_present_divider( reg, show_present_divider );
    gsr->sort_type = sort_code;
    gnc_ledger_display_refresh( gsr->ledger );
}

static void
gnc_split_reg_sort( GNCSplitReg *gsr, SortType sort_code )
{
    gnc_split_reg_sort_force( gsr, sort_code, FALSE );
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
gnc_split_reg_set_sort_reversed(GNCSplitReg *gsr, gboolean rev, gboolean refresh)
{
    /* Note: sort_reversed is the boolean opposite of sort_increasing
     *       so when rev == true, we're sorting decreasing
     *       In other words, qof_query_set_sort_increasing should
     *       always use the inverse of rev.
     */
    Query *query = gnc_ledger_display_get_query( gsr->ledger );
    qof_query_set_sort_increasing (query, !rev, !rev, !rev);
    gsr->sort_rev = rev;
    if (refresh)
        gnc_ledger_display_refresh( gsr->ledger );
}

static gboolean
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
        return FALSE;
    }

    gsr_emit_include_date_signal( gsr, xaccTransGetDate(trans) );

    /* Explicit redraw shouldn't be needed,
     * since gui_refresh events should handle this. */
    /* gnc_split_register_redraw (reg); */
    LEAVE(" ");
    return TRUE;
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

    goto_blank = gnc_prefs_get_bool(GNC_PREFS_GROUP_GENERAL_REGISTER,
                                    GNC_PREF_ENTER_MOVES_TO_END);

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
    if (!gnc_split_reg_record (gsr))
    {
        /* we may come here from the transfer cell if we decline to create a
         * new account, make sure the sheet has the focus if the record is FALSE
         * which results in no cursor movement. */
        gnc_split_reg_focus_on_sheet (gsr);

        /* if there are no changes, just enter was pressed, proceed to move
         * other wise lets not move. */
        if (gnc_table_current_cursor_changed (sr->table, FALSE))
        {
            LEAVE(" ");
            return;
        }
    }

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
add_summary_label (GtkWidget *summarybar, gboolean pack_start, const char *label_str, GtkWidget *extra)
{
    GtkWidget *hbox;
    GtkWidget *label;

    hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 2);
    gtk_box_set_homogeneous (GTK_BOX (hbox), FALSE);
    if (pack_start)
        gtk_box_pack_start( GTK_BOX(summarybar), hbox, FALSE, FALSE, 5 );
    else
        gtk_box_pack_end( GTK_BOX(summarybar), hbox, FALSE, FALSE, 5 );

    label = gtk_label_new( label_str );
    gnc_label_set_alignment(label, 1.0, 0.5 );
    gtk_box_pack_start( GTK_BOX(hbox), label, FALSE, FALSE, 0 );

    label = gtk_label_new( "" );
    gnc_label_set_alignment(label, 1.0, 0.5 );
    gtk_box_pack_start( GTK_BOX(hbox), label, FALSE, FALSE, 0 );

    if (extra != NULL)
        gtk_box_pack_start( GTK_BOX(hbox), extra, FALSE, FALSE, 0 );

    return label;
}

static void
gsr_summarybar_set_arrow_draw (GNCSplitReg *gsr)
{
    if (gsr->sort_arrow_handler_id > 0)
       g_signal_handler_disconnect (G_OBJECT(gsr->sort_arrow), gsr->sort_arrow_handler_id);

    gsr->sort_arrow_handler_id = g_signal_connect (G_OBJECT (gsr->sort_arrow), "draw",
                                                   G_CALLBACK (gnc_draw_arrow_cb), GINT_TO_POINTER(gsr->sort_rev));

    gtk_widget_queue_draw (gsr->sort_arrow);
}

GtkWidget *
gsr_create_summary_bar( GNCSplitReg *gsr )
{
    GtkWidget *summarybar = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 4);
    gtk_box_set_homogeneous (GTK_BOX (summarybar), FALSE);
    gtk_widget_set_name (summarybar, "gnc-id-summarybar");

    gsr->cleared_label    = NULL;
    gsr->balance_label    = NULL;
    gsr->reconciled_label = NULL;
    gsr->future_label     = NULL;
    gsr->projectedminimum_label  = NULL;
    gsr->sort_label       = NULL;
    gsr->sort_arrow       = NULL;
    gsr->filter_label     = NULL;
    gsr->shares_label     = NULL;
    gsr->value_label      = NULL;

    if (gnc_ledger_display_type(gsr->ledger) == LD_SINGLE)
    {
        if (!xaccAccountIsPriced(gnc_ledger_display_leader(gsr->ledger)))
        {
            gsr->balance_label    = add_summary_label (summarybar, TRUE, _("Present:"), NULL);
            gsr->future_label     = add_summary_label (summarybar, TRUE, _("Future:"), NULL);
            gsr->cleared_label    = add_summary_label (summarybar, TRUE, _("Cleared:"), NULL);
            gsr->reconciled_label = add_summary_label (summarybar, TRUE, _("Reconciled:"), NULL);
            gsr->projectedminimum_label  = add_summary_label (summarybar, TRUE, _("Projected Minimum:"), NULL);
        }
        else
        {
            gsr->shares_label     = add_summary_label (summarybar, TRUE, _("Shares:"), NULL);
            gsr->value_label      = add_summary_label (summarybar, TRUE, _("Current Value:"), NULL);
        }
    }

    gsr->filter_label = add_summary_label (summarybar, FALSE, "", NULL);
    gsr->sort_arrow = gtk_image_new_from_icon_name ("image-missing", GTK_ICON_SIZE_SMALL_TOOLBAR);
    gsr->sort_label = add_summary_label (summarybar, FALSE, _("Sort By:"), gsr->sort_arrow);

    gnc_widget_style_context_add_class (GTK_WIDGET(gsr->filter_label), "gnc-class-highlight");
    gnc_widget_style_context_add_class (GTK_WIDGET(gsr->sort_arrow), "gnc-class-highlight");

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
    case GENERAL_JOURNAL:
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
    gnc_dialog_run(GTK_DIALOG(dialog), GNC_PREF_WARN_REG_IS_READ_ONLY);
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
    SplitRegister *reg;

    if (qof_book_is_readonly(gnc_get_current_book()))
    {
        /* Is the book read-only? Then for sure also make this register
        read-only. */
        gsr->read_only = TRUE;
    }

    if ( !gsr->read_only )
    {
        dialog_args *args;
        char *string = NULL;
        switch (gnc_split_reg_get_placeholder(gsr))
        {
        case PLACEHOLDER_NONE:
            /* stay as false. */
            return;

        case PLACEHOLDER_THIS:
            string = _("This account may not be edited. If you want "
                             "to edit transactions in this register, please "
                             "open the account options and turn off the "
                             "placeholder checkbox.");
            break;

        default:
            string = _("One of the sub-accounts selected may not be "
                             "edited. If you want to edit transactions in "
                             "this register, please open the sub-account "
                             "options and turn off the placeholder checkbox. "
                             "You may also open an individual account instead "
                             "of a set of accounts.");
            break;
        }
        gsr->read_only = TRUE;
        /* Put up a warning dialog */
        args = g_malloc(sizeof(dialog_args));
        args->string = string;
        args->gsr = gsr;
        g_timeout_add (250, gtk_callback_bug_workaround, args); /* 0.25 seconds */
    }

    /* Make the contents immutable */
    reg = gnc_ledger_display_get_split_register( gsr->ledger );
    gnc_split_register_set_read_only( reg, TRUE );

}

static
GtkWidget *
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
gsr_emit_show_popup_menu( GnucashRegister *reg, gpointer user_data )
{
    gsr_emit_simple_signal( (GNCSplitReg*)user_data, "show-popup-menu" );
}

static
void
gsr_emit_include_date_signal( GNCSplitReg *gsr, time64 date )
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

void
gnc_split_reg_set_sort_type_force( GNCSplitReg *gsr, SortType t, gboolean force )
{
    gnc_split_reg_sort_force( gsr, t, force );
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
