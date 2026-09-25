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
#include "dialog-doclink.h"
#include "dialog-doclink-utils.h"
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
#include "split-register-p.h"
#include "table-control.h"
#include "table-allgui.h"
#include "gnc-state.h"

#include "dialog-utils.h"

// static QofLogModule log_module = GNC_MOD_SX;
static QofLogModule log_module = GNC_MOD_GUI;

/***** PROTOTYPES ***************************************************/
void gnc_split_reg_raise( GNCSplitReg *gsr );

static GtkWidget* add_summary_label( GtkWidget *summarybar, gboolean pack_start,
                                     const char *label_str, GtkWidget *extra );

static void gsr_summarybar_set_arrow_icon (GNCSplitReg *gsr);

static void gnc_split_reg_determine_read_only( GNCSplitReg *gsr, gboolean show_dialog );
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
void gsr_default_doclink_handler ( GNCSplitReg *w );
void gsr_default_doclink_open_handler ( GNCSplitReg *w );
void gsr_default_doclink_remove_handler ( GNCSplitReg *w );
static void gsr_default_doclink_from_sheet_handler ( GNCSplitReg *w );

static void gsr_emit_simple_signal       ( GNCSplitReg *gsr, const char *sigName );
static void gsr_emit_help_changed        ( GnucashRegister *reg, gpointer user_data );
static void gsr_emit_show_popup_menu     ( GnucashRegister *reg, gpointer user_data );

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


static void gnc_split_reg_init2( GNCSplitReg *gsr );
void gnc_split_reg_dispose(GObject *obj);

FROM_STRING_FUNC(SortType, ENUM_LIST_SORTTYPE)
AS_STRING_FUNC(SortType, ENUM_LIST_SORTTYPE)

G_DEFINE_TYPE (GNCSplitReg, gnc_split_reg, GTK_TYPE_BOX)

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
        { LAST_SIGNAL, NULL, 0 }
    };

    object_class = (GObjectClass*) klass;

    for ( i = 0; signals[i].s != LAST_SIGNAL; i++ )
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

    gnc_split_reg_determine_read_only( gsr, TRUE );

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

static void
gsr_move_sort_and_filter_to_state_file (GNCSplitReg *gsr, GKeyFile* state_file, const gchar *state_section)
{
    GNCLedgerDisplayType ledger_type;

    // Look for any old kvp entries and add them to .gcm file
    ledger_type = gnc_ledger_display_type (gsr->ledger);

    // General ledger should already be using .gcm file
    if ((ledger_type == LD_SINGLE) || (ledger_type == LD_SUBACCOUNT))
    {
        Account *leader = gnc_ledger_display_leader (gsr->ledger);
        const char* kvp_filter = NULL;
        const char* kvp_sort_order = NULL;
        gboolean kvp_sort_reversed = FALSE;

        kvp_filter = xaccAccountGetFilter (leader);
        if (kvp_filter)
        {
            gchar *temp_filter_text = g_strdup (kvp_filter);
            // make it conform to .gcm file list
            g_strdelimit (temp_filter_text, ",", ';');
            g_key_file_set_string (state_file, state_section, KEY_PAGE_FILTER,
                                   temp_filter_text);
            g_free (temp_filter_text);
            xaccAccountSetFilter (leader, NULL);
        }

        kvp_sort_order = xaccAccountGetSortOrder (leader);
        if (kvp_sort_order)
        {
            g_key_file_set_string (state_file, state_section,
                                   KEY_PAGE_SORT, kvp_sort_order);
            xaccAccountSetSortOrder (leader, NULL);
        }

        kvp_sort_reversed = xaccAccountGetSortReversed (leader);
        if (kvp_sort_reversed)
        {
            g_key_file_set_boolean (state_file, state_section,
                                    KEY_PAGE_SORT_REV, kvp_sort_reversed);
            xaccAccountSetSortReversed (leader, FALSE);
        }
    }
}

gchar *
gsr_get_register_state_section (GNCSplitReg *gsr)
{
    GNCLedgerDisplayType ledger_type = gnc_ledger_display_type (gsr->ledger);
    Account *account = gnc_ledger_display_leader (gsr->ledger);
    const GncGUID *guid = xaccAccountGetGUID (account);
    gchar guidstr[GUID_ENCODING_LENGTH+1];
    gchar *register_state_section;

    guid_to_string_buff (guid, guidstr);

    if (ledger_type == LD_SUBACCOUNT)
        register_state_section = g_strconcat (STATE_SECTION_REG_PREFIX, " ", guidstr, "+", NULL);
    else
        register_state_section = g_strconcat (STATE_SECTION_REG_PREFIX, " ", guidstr, NULL);

    return register_state_section;
}

static
void
gsr_create_table( GNCSplitReg *gsr )
{
    GtkWidget *register_widget = NULL;
    SplitRegister *sr = NULL;
    GKeyFile* state_file = gnc_state_get_current();
    gchar *register_state_section;

    /* register_state_section is used to store per register state: column widths, sort order,... */
    register_state_section = gsr_get_register_state_section (gsr);

    ENTER("gsr=%p", gsr);

    sr = gnc_ledger_display_get_split_register (gsr->ledger);

    gnc_ledger_display_set_user_data( gsr->ledger, (gpointer)gsr );
    gnc_ledger_display_set_handlers( gsr->ledger,
                                     gnc_split_reg_ld_destroy,
                                     gnc_split_reg_get_parent );

    /* FIXME: We'd really rather pass this down... */
    sr = gnc_ledger_display_get_split_register( gsr->ledger );
    register_widget = gnucash_register_new( sr->table, register_state_section );
    gsr->reg = GNUCASH_REGISTER( register_widget );

    gtk_box_append (GTK_BOX(gsr), GTK_WIDGET(gsr->reg));
    gnucash_sheet_set_window (gnucash_register_get_sheet (gsr->reg), gsr->window);

    // setup the callback for when the doclink cell clicked on
    gnucash_register_set_open_doclink_cb (gsr->reg,
        (GFunc)gsr_default_doclink_from_sheet_handler, gsr);

    gtk_widget_set_visible (GTK_WIDGET(gsr->reg), TRUE);
    g_signal_connect (gsr->reg, "activate_cursor",
                      G_CALLBACK(gnc_split_reg_record_cb), gsr);
    g_signal_connect (gsr->reg, "redraw_all",
                      G_CALLBACK(gsr_redraw_all_cb), gsr);
    g_signal_connect (gsr->reg, "redraw_help",
                      G_CALLBACK(gsr_emit_help_changed), gsr);
    g_signal_connect (gsr->reg, "show_popup_menu",
                      G_CALLBACK(gsr_emit_show_popup_menu), gsr);

    gsr_move_sort_and_filter_to_state_file (gsr, state_file, register_state_section);

    g_free (register_state_section);
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
        g_signal_handlers_disconnect_by_data (gsr->reg, gsr);
    gsr->reg = NULL;

    /* GNCSplitReg owns the register as a GtkBox child. Chaining disposal lets
     * GtkBox unparent it exactly once; it is not a separate GtkWindow. */
    G_OBJECT_CLASS (gnc_split_reg_parent_class)->dispose (obj);
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
 * Duplicate-code reduction function; retrieves, formats and updates the
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
    const gchar *label_str = NULL;
    GtkWidget *text_label, *hbox;
    gchar *bidi_string;

    if ( label == NULL )
        return;

    hbox = g_object_get_data (G_OBJECT(label), "text_box");
    text_label = g_object_get_data (G_OBJECT(label), "text_label");
    label_str = gtk_label_get_text (GTK_LABEL(text_label));

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
    bidi_string = gnc_wrap_text_with_bidi_ltr_isolate (string);
    gtk_label_set_text( GTK_LABEL(label), bidi_string );
    g_free (bidi_string);

    if (label_str)
    {
        gchar *tooltip = g_strdup_printf ("%s %s", label_str, string);
        gtk_widget_set_tooltip_text (GTK_WIDGET(hbox), tooltip);
        g_free (tooltip);
    }
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

    /* no EURO conversion, if account is already EURO or no EURO currency */
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
        const gchar *old_tt_text = gtk_widget_get_tooltip_text (GTK_WIDGET(gsr->sort_label));
        const gchar *new_tt_text;
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
            gsr_summarybar_set_arrow_icon (gsr);

        gtk_label_set_text (GTK_LABEL(gsr->sort_label), text);
    }

    // Filter label
    if (gsr->filter_label != NULL)
    {
        const gchar *old_tt_text = gtk_widget_get_tooltip_text (GTK_WIDGET(gsr->filter_label));

        // check for a change in text
        if (g_strcmp0 (old_tt_text, gsr->filter_text) != 0)
        {
            if (gsr->filter_text != NULL)
                gtk_label_set_text (GTK_LABEL(gsr->filter_label), _("Filtered"));
            else
                gtk_label_set_text (GTK_LABEL(gsr->filter_label), "");

            gtk_widget_set_tooltip_text (GTK_WIDGET(gsr->filter_label), gsr->filter_text);

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

    if (gsr)
    {
        /* register_state_section is used to store per register state: column widths, sort order,... */
        gchar *register_state_section = gsr_get_register_state_section (gsr);
        SplitRegister *reg = gnc_ledger_display_get_split_register (ledger);

        if (reg && reg->table)
            gnc_table_save_state (reg->table, register_state_section);

        /*
         * Don't destroy the window here any more.  The register no longer
         * owns it.
         */
        g_free (register_state_section);
    }

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

typedef enum
{
    GSR_MUTATION_CUT,
    GSR_MUTATION_DELETE,
    GSR_MUTATION_REINITIALIZE
} GsrMutationAction;

typedef struct
{
    GncSplitRegisterAsyncRequest base;
    GWeakRef gsr;
    GWeakRef window;
    QofBook *book;
    GncGUID transaction_guid;
    GncGUID split_guid;
    GncGUID trans_split_guid;
    VirtualLocation cursor_loc;
    CursorClass cursor_class;
    GsrMutationAction action;
    gboolean has_trans_split;
    gboolean cancelled;
} GsrMutationRequest;

static void
gsr_warning_dialog_finished (G_GNUC_UNUSED gint response,
                             G_GNUC_UNUSED gpointer user_data)
{
}

static void
gsr_mutation_request_free (GsrMutationRequest *request)
{
    gnc_split_register_async_request_untrack (&request->base);
    g_weak_ref_clear (&request->window);
    g_weak_ref_clear (&request->gsr);
    g_free (request);
}

static void
gsr_mutation_request_cancel (GncSplitRegisterAsyncRequest *base)
{
    GsrMutationRequest *request = (GsrMutationRequest *)base;
    SplitRegister *reg = request->base.reg;

    request->cancelled = TRUE;
    if (reg && reg->table && reg->table->control)
        gnc_table_control_set_input_suspended (reg->table->control, FALSE);
    /* The warning owns its completion callback. It releases this detached
     * request after it has observed the cancellation. */
    gnc_split_register_async_request_untrack (&request->base);
}

static gboolean
gsr_mutation_request_context (GsrMutationRequest *request,
                              GObject **owner_out,
                              GtkWindow **parent_out,
                              Transaction **transaction_out,
                              Split **split_out,
                              Split **trans_split_out)
{
    GObject *owner = g_weak_ref_get (&request->gsr);
    GObject *window = g_weak_ref_get (&request->window);
    SplitRegister *reg = request->base.reg;
    GNCSplitReg *gsr;
    Transaction *transaction;
    Split *split;
    Split *trans_split = NULL;

    if (request->cancelled || !reg || !reg->table || !owner || !window ||
        !IS_GNC_SPLIT_REG (owner) || !GTK_IS_WINDOW (window))
        goto out;

    gsr = GNC_SPLIT_REG (owner);
    if (gsr->window != GTK_WIDGET (window) || !gsr->ledger ||
        gnc_ledger_display_get_split_register (gsr->ledger) != reg ||
        request->book != gnc_get_current_book () ||
        !virt_loc_equal (reg->table->current_cursor_loc, request->cursor_loc) ||
        gnc_split_register_get_current_cursor_class (reg) != request->cursor_class)
        goto out;

    transaction = gnc_split_register_get_current_trans (reg);
    split = gnc_split_register_get_current_split (reg);
    if (!transaction || !split || xaccSplitGetParent (split) != transaction ||
        !guid_equal (xaccTransGetGUID (transaction), &request->transaction_guid) ||
        !guid_equal (xaccSplitGetGUID (split), &request->split_guid))
        goto out;

    if (request->has_trans_split)
    {
        trans_split = xaccSplitLookup (&request->trans_split_guid, request->book);
        if (!trans_split || xaccSplitGetParent (trans_split) != transaction)
            goto out;
    }

    *owner_out = owner;
    *parent_out = GTK_WINDOW (window);
    *transaction_out = transaction;
    *split_out = split;
    *trans_split_out = trans_split;
    return TRUE;

out:
    g_clear_object (&window);
    g_clear_object (&owner);
    return FALSE;
}

static gboolean
gsr_mutation_prepare_blank_split (SplitRegister *reg, Split *split)
{
    if (gnc_split_register_is_blank_split (reg, split))
        gnc_split_register_change_blank_split_ref (reg, split);

    if (split == gnc_split_register_get_blank_split (reg))
    {
        gnc_split_register_cancel_cursor_trans_changes (reg);
        return FALSE;
    }
    return TRUE;
}

static void
gsr_mutation_request_apply (GsrMutationRequest *request)
{
    GObject *owner;
    GtkWindow *parent;
    Transaction *transaction;
    Split *split;
    Split *trans_split;
    SplitRegister *reg = request->base.reg;

    if (!gsr_mutation_request_context (request, &owner, &parent, &transaction,
                                       &split, &trans_split))
        return;

    if (!is_trans_readonly_and_warn (parent, transaction))
    {
        switch (request->action)
        {
        case GSR_MUTATION_CUT:
            if (gsr_mutation_prepare_blank_split (reg, split))
                gnc_split_register_cut_current (reg);
            break;
        case GSR_MUTATION_DELETE:
            if (gsr_mutation_prepare_blank_split (reg, split))
            {
                if (request->cursor_class == CURSOR_CLASS_SPLIT)
                    gnc_split_register_delete_current_split (reg);
                else if (request->cursor_class == CURSOR_CLASS_TRANS)
                    gnc_split_register_delete_current_trans (reg);
            }
            break;
        case GSR_MUTATION_REINITIALIZE:
            gnc_split_register_empty_current_trans_except_split (reg, trans_split);
            break;
        }
    }

    g_object_unref (parent);
    g_object_unref (owner);
}

static void
gsr_mutation_request_finished (gint response, gpointer user_data)
{
    GsrMutationRequest *request = user_data;
    SplitRegister *reg = request->base.reg;

    if (reg && reg->table && reg->table->control)
        gnc_table_control_set_input_suspended (reg->table->control, FALSE);
    if (response == GTK_RESPONSE_ACCEPT)
        gsr_mutation_request_apply (request);
    gsr_mutation_request_free (request);
}

static gboolean
gsr_mutation_request_start (GNCSplitReg *gsr, SplitRegister *reg,
                            Transaction *transaction, Split *split,
                            Split *trans_split, CursorClass cursor_class,
                            GsrMutationAction action, const gchar *warning,
                            const gchar *title, const gchar *message,
                            const gchar *button_label)
{
    GsrMutationRequest *request;
    GtkWindow *parent;

    if (!gsr || !reg || !reg->table || !transaction || !split ||
        !GTK_IS_WINDOW (gsr->window) || !gnc_split_register_get_info (reg) ||
        (action == GSR_MUTATION_REINITIALIZE && !trans_split) ||
        (reg->table->control &&
         gnc_table_control_input_suspended (reg->table->control)))
        return FALSE;

    parent = GTK_WINDOW (gsr->window);
    request = g_new0 (GsrMutationRequest, 1);
    request->book = gnc_get_current_book ();
    request->transaction_guid = *xaccTransGetGUID (transaction);
    request->split_guid = *xaccSplitGetGUID (split);
    request->cursor_loc = reg->table->current_cursor_loc;
    request->cursor_class = cursor_class;
    request->action = action;
    request->has_trans_split = trans_split != NULL;
    if (trans_split)
        request->trans_split_guid = *xaccSplitGetGUID (trans_split);
    g_weak_ref_init (&request->gsr, G_OBJECT (gsr));
    g_weak_ref_init (&request->window, G_OBJECT (parent));
    gnc_split_register_async_request_track (reg, &request->base,
                                             gsr_mutation_request_cancel);
    if (reg->table->control)
        gnc_table_control_set_input_suspended (reg->table->control, TRUE);
    gnc_warning_dialog_async (parent, warning, title, message, button_label,
                              GTK_RESPONSE_ACCEPT, TRUE,
                              gsr_mutation_request_finished, request);
    return TRUE;
}

static gboolean
gsr_prepare_mutation (GNCSplitReg *gsr, gboolean cancel_split_changes,
                      SplitRegister **reg_out, Transaction **transaction_out,
                      Split **split_out, CursorClass *cursor_class_out)
{
    SplitRegister *reg;
    Transaction *transaction;
    Split *split;
    CursorClass cursor_class;

    if (!gsr || !gsr->ledger || !GTK_IS_WINDOW (gsr->window))
        return FALSE;
    reg = gnc_ledger_display_get_split_register (gsr->ledger);
    if (!reg || !reg->table ||
        (reg->table->control &&
         gnc_table_control_input_suspended (reg->table->control)))
        return FALSE;

    split = gnc_split_register_get_current_split (reg);
    if (!split)
    {
        if (cancel_split_changes)
            gnc_split_register_cancel_cursor_split_changes (reg);
        return FALSE;
    }
    transaction = xaccSplitGetParent (split);
    cursor_class = gnc_split_register_get_current_cursor_class (reg);
    if (!transaction || cursor_class == CURSOR_CLASS_NONE ||
        is_trans_readonly_and_warn (GTK_WINDOW (gsr->window), transaction))
        return FALSE;

    *reg_out = reg;
    *transaction_out = transaction;
    *split_out = split;
    *cursor_class_out = cursor_class;
    return TRUE;
}

static void
gsr_show_anchor_error (GNCSplitReg *gsr, const gchar *title,
                       const gchar *detail)
{
    if (gsr && GTK_IS_WINDOW (gsr->window))
        gnc_error_dialog (GTK_WINDOW (gsr->window), "%s\n\n%s", title, detail);
}

void
gsr_default_cut_txn_handler (GNCSplitReg *gsr, gpointer data)
{
    CursorClass cursor_class;
    SplitRegister *reg;
    Transaction *transaction;
    Split *split;

    if (!gsr_prepare_mutation (gsr, TRUE, &reg, &transaction, &split,
                               &cursor_class))
        return;

    if (cursor_class == CURSOR_CLASS_SPLIT)
    {
        const gchar *format = _("Cut the split '%s' from the transaction '%s'?");
        const gchar *recn_warn = _("You would be removing a reconciled split! "
                                  "This is not a good idea as it will cause your "
                                  "reconciled balance to be off.");
        const gchar *anchor_error = _("You cannot cut this split.");
        const gchar *anchor_split = _("This is the split anchoring this transaction "
                                     "to the register. You may not remove it from "
                                     "this register window. You may remove the "
                                     "entire transaction from this window, or you "
                                     "may navigate to a register that shows "
                                     "another side of this same transaction and "
                                     "remove the split from that register.");
        const gchar *memo;
        const gchar *description;
        const gchar *warning;
        const gchar *message;
        gchar *title;
        char recn;

        if (reg->type != GENERAL_JOURNAL &&
            split == gnc_split_register_get_current_trans_split (reg, NULL))
        {
            gsr_show_anchor_error (gsr, anchor_error, anchor_split);
            return;
        }

        memo = xaccSplitGetMemo (split);
        memo = (memo && *memo) ? memo : _("(no memo)");
        description = xaccTransGetDescription (transaction);
        description = (description && *description) ? description : _("(no description)");
        title = g_strdup_printf (format, memo, description);
        recn = xaccSplitGetReconcile (split);
        warning = (recn == YREC || recn == FREC)
            ? GNC_PREF_WARN_REG_SPLIT_CUT_RECD : GNC_PREF_WARN_REG_SPLIT_CUT;
        message = (recn == YREC || recn == FREC) ? recn_warn : "";
        gsr_mutation_request_start (gsr, reg, transaction, split, NULL,
                                    cursor_class, GSR_MUTATION_CUT, warning,
                                    title, message, _("_Cut Split"));
        g_free (title);
        return;
    }

    if (cursor_class == CURSOR_CLASS_TRANS)
    {
        const gchar *recn_warn = _("You would be removing a transaction "
                                  "with reconciled splits! "
                                  "This is not a good idea as it will cause your "
                                  "reconciled balance to be off.");
        const gboolean reconciled = xaccTransHasReconciledSplits (transaction);

        gsr_mutation_request_start
            (gsr, reg, transaction, split, NULL, cursor_class, GSR_MUTATION_CUT,
             reconciled ? GNC_PREF_WARN_REG_TRANS_CUT_RECD : GNC_PREF_WARN_REG_TRANS_CUT,
             _("Cut the current transaction?"), reconciled ? recn_warn : "",
             _("_Cut Transaction"));
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
    const gchar *reason;
    const gchar *title = _("Cannot modify or delete this transaction.");
    const gchar *message =
        _("This transaction is marked read-only with the comment: '%s'");

    if (!trans)
        return FALSE;

    if (xaccTransIsReadonlyByPostedDate (trans))
    {
        gnc_error_dialog
            (parent, "%s\n\n%s", title,
             _("The date of this transaction is older than the \"Read-Only Threshold\" set for this book. "
               "This setting can be changed in File->Properties->Accounts."));
        return TRUE;
    }

    reason = xaccTransGetReadOnly (trans);
    if (reason)
    {
        gchar *detail = g_strdup_printf (message, reason);
        gnc_error_dialog (parent, "%s\n\n%s", title, detail);
        g_free (detail);
        return TRUE;
    }
    return FALSE;
}

void
gsr_default_reinit_handler (GNCSplitReg *gsr, gpointer data)
{
    VirtualCellLocation vcell_loc;
    CursorClass cursor_class;
    SplitRegister *reg;
    Transaction *transaction;
    Split *split;
    Split *trans_split;
    const gchar *recn_warn = _("This transaction contains reconciled splits. "
                              "Modifying it is not a good idea because that will "
                              "cause your reconciled balance to be off.");
    gboolean reconciled;

    if (!gsr_prepare_mutation (gsr, FALSE, &reg, &transaction, &split,
                               &cursor_class) ||
        !gnc_split_register_get_split_virt_loc (reg, split, &vcell_loc))
        return;

    trans_split = gnc_split_register_get_current_trans_split (reg, &vcell_loc);
    if (!trans_split || xaccSplitGetParent (trans_split) != transaction)
        return;

    reconciled = xaccTransHasReconciledSplits (transaction);
    gsr_mutation_request_start
        (gsr, reg, transaction, split, trans_split, cursor_class,
         GSR_MUTATION_REINITIALIZE,
         reconciled ? GNC_PREF_WARN_REG_SPLIT_DEL_ALL_RECD : GNC_PREF_WARN_REG_SPLIT_DEL_ALL,
         _("Remove the splits from this transaction?"),
         reconciled ? recn_warn : "", _("_Remove Splits"));
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

typedef struct
{
    GWeakRef gsr;
    GWeakRef window;
    GNCLedgerDisplay *ledger;
    QofBook *book;
    GncGUID transaction_guid;
} GsrDoclinkUpdateRequest;

static void
gsr_doclink_update_request_free (GsrDoclinkUpdateRequest *request)
{
    g_weak_ref_clear (&request->window);
    g_weak_ref_clear (&request->gsr);
    g_free (request);
}

static gboolean
gsr_doclink_update_request_context (GsrDoclinkUpdateRequest *request,
                                    GObject **owner_out,
                                    GtkWindow **parent_out,
                                    Transaction **transaction_out)
{
    GObject *owner = g_weak_ref_get (&request->gsr);
    GObject *window = g_weak_ref_get (&request->window);
    GNCSplitReg *gsr;
    Transaction *transaction;

    if (!owner || !window || !IS_GNC_SPLIT_REG (owner) || !GTK_IS_WINDOW (window))
        goto out;

    gsr = GNC_SPLIT_REG (owner);
    if (gsr->window != GTK_WIDGET (window) || gsr->ledger != request->ledger ||
        request->book != gnc_get_current_book ())
        goto out;

    transaction = xaccTransLookup (&request->transaction_guid, request->book);
    if (!transaction)
        goto out;

    *owner_out = owner;
    *parent_out = GTK_WINDOW (window);
    *transaction_out = transaction;
    return TRUE;

out:
    g_clear_object (&window);
    g_clear_object (&owner);
    return FALSE;
}

static void
update_trans_uri_gui_destroy_cb (G_GNUC_UNUSED GtkWidget *object,
                                 gpointer user_data)
{
    DoclinkReturn *dlr = user_data;
    GsrDoclinkUpdateRequest *request = dlr->user_data;
    GObject *owner;
    GtkWindow *parent;
    Transaction *transaction;

    if (dlr->response != GTK_RESPONSE_CANCEL && dlr->updated_uri &&
        g_strcmp0 (dlr->existing_uri, dlr->updated_uri) != 0 &&
        gsr_doclink_update_request_context (request, &owner, &parent, &transaction))
    {
        if (!is_trans_readonly_and_warn (parent, transaction))
            xaccTransSetDocLink (transaction, dlr->updated_uri);
        g_object_unref (parent);
        g_object_unref (owner);
    }

    gsr_doclink_update_request_free (request);
    g_free (dlr->existing_uri);
    g_free (dlr->updated_uri);
    g_free (dlr);
}

/* Edit the document link for the current transaction. */
void
gsr_default_doclink_handler (GNCSplitReg *gsr)
{
    SplitRegister *reg;
    Split *split;
    Transaction *transaction;
    CursorClass cursor_class;
    gchar *uri;
    DoclinkReturn *dlr;
    GsrDoclinkUpdateRequest *request;
    GtkWidget *window;

    if (!gsr || !gsr->ledger || !GTK_IS_WINDOW (gsr->window))
        return;
    reg = gnc_ledger_display_get_split_register (gsr->ledger);
    split = reg ? gnc_split_register_get_current_split (reg) : NULL;
    if (!split)
    {
        if (reg)
            gnc_split_register_cancel_cursor_split_changes (reg);
        return;
    }

    transaction = xaccSplitGetParent (split);
    cursor_class = gnc_split_register_get_current_cursor_class (reg);
    if (!transaction || cursor_class == CURSOR_CLASS_NONE ||
        is_trans_readonly_and_warn (GTK_WINDOW (gsr->window), transaction))
        return;

    uri = gnc_doclink_convert_trans_link_uri (transaction, gsr->read_only);
    request = g_new0 (GsrDoclinkUpdateRequest, 1);
    request->ledger = gsr->ledger;
    request->book = gnc_get_current_book ();
    request->transaction_guid = *xaccTransGetGUID (transaction);
    g_weak_ref_init (&request->gsr, G_OBJECT (gsr));
    g_weak_ref_init (&request->window, G_OBJECT (gsr->window));

    dlr = g_new0 (DoclinkReturn, 1);
    dlr->existing_uri = g_strdup (uri);
    dlr->user_data = request;

    window = gnc_doclink_get_uri_dialog (GTK_WINDOW (gsr->window),
                                         _("Change a Transaction Linked Document"),
                                         dlr);
    if (window)
        g_signal_connect (window, "destroy",
                          G_CALLBACK (update_trans_uri_gui_destroy_cb), dlr);
    else
        update_trans_uri_gui_destroy_cb (NULL, dlr);

    g_free (uri);
}
/* Opens the document link for the current transaction. */
void
gsr_default_doclink_open_handler (GNCSplitReg *gsr)
{
    CursorClass cursor_class;
    SplitRegister *reg = gnc_ledger_display_get_split_register (gsr->ledger);
    Transaction *trans;
    Split *split = gnc_split_register_get_current_split (reg);
    gchar *uri;

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

    // fix an earlier error when storing relative paths before version 3.5
    uri = gnc_doclink_convert_trans_link_uri (trans, gsr->read_only);

    gnc_doclink_open_uri (GTK_WINDOW (gsr->window), uri);
    g_free (uri);
}

/* Removes the document link for the current transaction. */
void
gsr_default_doclink_remove_handler (GNCSplitReg *gsr)
{
    CursorClass cursor_class;
    SplitRegister *reg = gnc_ledger_display_get_split_register (gsr->ledger);
    Transaction *trans;
    Split *split = gnc_split_register_get_current_split (reg);

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

    if (is_trans_readonly_and_warn (GTK_WINDOW(gsr->window), trans))
        return;

    xaccTransSetDocLink (trans, "");
}

static void
gsr_default_doclink_from_sheet_handler (GNCSplitReg *gsr)
{
    SplitRegister *reg = gnc_ledger_display_get_split_register (gsr->ledger);
    Transaction *trans;
    Split *split;
    gchar *uri = NULL;

    /* get the current split based on cursor position */
    split = gnc_split_register_get_current_split (reg);
    if (!split)
        return;

    trans = xaccSplitGetParent (split);

    // fix an earlier error when storing relative paths before version 3.5
    uri = gnc_doclink_convert_trans_link_uri (trans, gsr->read_only);

    if (uri)
        gnc_doclink_open_uri (GTK_WINDOW (gsr->window), uri);

    g_free (uri);
}

void
gsr_default_delete_handler (GNCSplitReg *gsr, gpointer data)
{
    CursorClass cursor_class;
    SplitRegister *reg;
    Transaction *transaction;
    Split *split;

    if (!gsr_prepare_mutation (gsr, TRUE, &reg, &transaction, &split,
                               &cursor_class))
        return;

    if (cursor_class == CURSOR_CLASS_SPLIT)
    {
        const gchar *format = _("Delete the split '%s' from the transaction '%s'?");
        const gchar *recn_warn = _("You would be deleting a reconciled split! "
                                  "This is not a good idea as it will cause your "
                                  "reconciled balance to be off.");
        const gchar *anchor_error = _("You cannot delete this split.");
        const gchar *anchor_split = _("This is the split anchoring this transaction "
                                     "to the register. You may not delete it from "
                                     "this register window. You may delete the "
                                     "entire transaction from this window, or you "
                                     "may navigate to a register that shows "
                                     "another side of this same transaction and "
                                     "delete the split from that register.");
        const gchar *memo;
        const gchar *description;
        const gchar *warning;
        const gchar *message;
        gchar *title;
        char recn;

        if (reg->type != GENERAL_JOURNAL &&
            split == gnc_split_register_get_current_trans_split (reg, NULL))
        {
            gsr_show_anchor_error (gsr, anchor_error, anchor_split);
            return;
        }

        memo = xaccSplitGetMemo (split);
        memo = (memo && *memo) ? memo : _("(no memo)");
        description = xaccTransGetDescription (transaction);
        description = (description && *description) ? description : _("(no description)");
        title = g_strdup_printf (format, memo, description);
        recn = xaccSplitGetReconcile (split);
        warning = (recn == YREC || recn == FREC)
            ? GNC_PREF_WARN_REG_SPLIT_DEL_RECD : GNC_PREF_WARN_REG_SPLIT_DEL;
        message = (recn == YREC || recn == FREC) ? recn_warn : "";
        gsr_mutation_request_start (gsr, reg, transaction, split, NULL,
                                    cursor_class, GSR_MUTATION_DELETE, warning,
                                    title, message, _("_Delete Split"));
        g_free (title);
        return;
    }

    if (cursor_class == CURSOR_CLASS_TRANS)
    {
        const gchar *recn_warn = _("You would be deleting a transaction "
                                  "with reconciled splits! "
                                  "This is not a good idea as it will cause your "
                                  "reconciled balance to be off.");
        const gboolean reconciled = xaccTransHasReconciledSplits (transaction);

        gsr_mutation_request_start
            (gsr, reg, transaction, split, NULL, cursor_class, GSR_MUTATION_DELETE,
             reconciled ? GNC_PREF_WARN_REG_TRANS_DEL_RECD : GNC_PREF_WARN_REG_TRANS_DEL,
             _("Delete the current transaction?"), reconciled ? recn_warn : "",
             _("_Delete Transaction"));
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
    gnc_split_register_duplicate_current_async (gnc_ledger_display_get_split_register (gsr->ledger), G_OBJECT (gsr));
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
    SplitRegister *reg;

    if (!gsr || !gsr->ledger)
        return;

    reg = gnc_ledger_display_get_split_register (gsr->ledger);
    if (!reg)
        return;

    /* The register model is the single source of truth. GTK4 reflects this
     * through SplitTransactionAction during redraw, so legacy menu and
     * toolbar signal paths must toggle the model instead of sampling widgets. */
    gnc_split_register_expand_current_trans
        (reg, !gnc_split_register_current_trans_expanded (reg));
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

typedef struct
{
    GncSplitRegisterAsyncRequest base;
    GWeakRef gsr;
    GWeakRef window;
    QofBook *book;
    GncGUID book_guid;
    GncGUID split_guid;
    GncSplitRegRevealCallback completed;
    gpointer user_data;
    GDestroyNotify destroy_notify;
    gboolean cancelled;
} GsrRevealSplitRequest;

static void
gsr_reveal_split_request_free (GsrRevealSplitRequest *request)
{
    GDestroyNotify destroy_notify;
    gpointer user_data;

    gnc_split_register_async_request_untrack (&request->base);
    g_weak_ref_clear (&request->window);
    g_weak_ref_clear (&request->gsr);

    destroy_notify = request->destroy_notify;
    user_data = request->user_data;
    request->destroy_notify = NULL;
    request->user_data = NULL;
    if (destroy_notify)
        destroy_notify (user_data);
    g_free (request);
}

static void
gsr_reveal_split_request_cancel (GncSplitRegisterAsyncRequest *base)
{
    GsrRevealSplitRequest *request = (GsrRevealSplitRequest *)base;
    SplitRegister *reg = request->base.reg;

    request->cancelled = TRUE;
    if (reg && reg->table && reg->table->control)
        gnc_table_control_set_input_suspended (reg->table->control, FALSE);
    /* GtkAlertDialog owns the completion callback. Keep this detached request
     * alive until it observes cancellation and invokes the destroy notifier. */
    gnc_split_register_async_request_untrack (&request->base);
}

static gboolean
gsr_reveal_split_request_context (GsrRevealSplitRequest *request,
                                  GNCSplitReg **gsr_out,
                                  GtkWindow **parent_out,
                                  Split **split_out)
{
    GObject *owner = g_weak_ref_get (&request->gsr);
    GObject *window = g_weak_ref_get (&request->window);
    SplitRegister *reg = request->base.reg;
    GNCSplitReg *gsr;
    QofBook *book;
    Split *split;

    if (request->cancelled || !reg || !reg->table || !owner || !window ||
        !IS_GNC_SPLIT_REG (owner) || !GTK_IS_WINDOW (window))
        goto out;

    gsr = GNC_SPLIT_REG (owner);
    book = gnc_get_current_book ();
    if (gsr->window != GTK_WIDGET (window) || !gsr->ledger ||
        gnc_ledger_display_get_split_register (gsr->ledger) != reg || !book ||
        request->book != book ||
        !guid_equal (&request->book_guid, qof_book_get_guid (book)) ||
        qof_book_shutting_down (book))
        goto out;

    split = xaccSplitLookup (&request->split_guid, request->book);
    if (!split)
        goto out;

    *gsr_out = gsr;
    *parent_out = GTK_WINDOW (window);
    *split_out = split;
    return TRUE;

out:
    g_clear_object (&window);
    g_clear_object (&owner);
    return FALSE;
}

static void
gsr_reveal_split_request_finished (GtkWindow *dialog, gint response, gpointer user_data)
{
    GsrRevealSplitRequest *request = user_data;
    SplitRegister *reg = request->base.reg;

    (void)dialog;

    if (reg && reg->table && reg->table->control)
        gnc_table_control_set_input_suspended (reg->table->control, FALSE);

    if (response == GTK_RESPONSE_OK)
    {
        GNCSplitReg *gsr;
        GtkWindow *parent;
        Split *split;

        if (gsr_reveal_split_request_context (request, &gsr, &parent, &split))
        {
            request->completed (gsr, split, GNC_SPLIT_REG_REVEAL_FILTER_CLEARED,
                                request->user_data);
            g_object_unref (parent);
            g_object_unref (gsr);
        }
    }
    gsr_reveal_split_request_free (request);
}

void
gnc_split_reg_reveal_split_async (GNCSplitReg *gsr, Split *split,
                                  GncSplitRegRevealCallback completed,
                                  gpointer user_data,
                                  GDestroyNotify destroy_notify)
{
    GsrRevealSplitRequest *request;
    SplitRegister *reg;
    VirtualCellLocation vcell_loc;
    GtkWindow *parent;
    QofBook *book;

    if (!completed || !gsr || !split || !gsr->ledger ||
        !GTK_IS_WINDOW (gsr->window))
        goto rejected;

    reg = gnc_ledger_display_get_split_register (gsr->ledger);
    parent = GTK_WINDOW (gsr->window);
    book = gnc_get_current_book ();
    if (!reg || !reg->table || !gnc_split_register_get_info (reg) || !book ||
        qof_book_shutting_down (book) ||
        xaccSplitLookup (xaccSplitGetGUID (split), book) != split)
        goto rejected;

    if (gnc_split_register_get_split_virt_loc (reg, split, &vcell_loc))
    {
        completed (gsr, split, GNC_SPLIT_REG_REVEAL_ALREADY_VISIBLE, user_data);
        if (destroy_notify)
            destroy_notify (user_data);
        return;
    }

    if (reg->table->control &&
        gnc_table_control_input_suspended (reg->table->control))
        goto rejected;

    request = g_new0 (GsrRevealSplitRequest, 1);
    request->book = book;
    request->book_guid = *qof_book_get_guid (book);
    request->split_guid = *xaccSplitGetGUID (split);
    request->completed = completed;
    request->user_data = user_data;
    request->destroy_notify = destroy_notify;
    g_weak_ref_init (&request->gsr, G_OBJECT (gsr));
    g_weak_ref_init (&request->window, G_OBJECT (parent));
    gnc_split_register_async_request_track (reg, &request->base,
                                            gsr_reveal_split_request_cancel);
    if (reg->table->control)
        gnc_table_control_set_input_suspended (reg->table->control, TRUE);
    gnc_ok_cancel_dialog_async
        (parent, GTK_RESPONSE_CANCEL, gsr_reveal_split_request_finished, request,
         _("Target split is currently hidden in this register.\n\n%s\n\n"
           "Select OK to temporarily clear filter and proceed,\n"
           "otherwise the last active cell will be selected."),
         gsr->filter_text ? gsr->filter_text : "");
    return;

rejected:
    if (destroy_notify)
        destroy_notify (user_data);
}

/**
 * move the cursor to the split, if present in register
**/
void
gnc_split_reg_jump_to_split(GNCSplitReg *gsr, Split *split)
{
    if (!gsr) return;

    SplitRegister *reg = gnc_ledger_display_get_split_register( gsr->ledger );

    VirtualCellLocation vcell_loc;
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
    if (!gsr) return;

    SplitRegister *reg = gnc_ledger_display_get_split_register (gsr->ledger);

    VirtualLocation virt_loc;
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
    {
        if ((vcell_loc.virt_row > 1) && (reg->style == REG_STYLE_JOURNAL))
            vcell_loc.virt_row--; // highlight the date field

        gnucash_register_goto_virt_cell (gsr->reg, vcell_loc);
    }
    gnc_ledger_display_refresh (gsr->ledger);
    LEAVE(" ");
}

void
gnc_split_reg_focus_on_sheet (GNCSplitReg *gsr)
{
    GnucashRegister *reg = gsr->reg;
    GnucashSheet *sheet = gnucash_register_get_sheet (reg);

    // Make sure the sheet is the focus only when it is realized
    if (!gtk_widget_has_focus(GTK_WIDGET(sheet)) && gtk_widget_get_realized (GTK_WIDGET(sheet)))
        gtk_widget_grab_focus (GTK_WIDGET(sheet));
}

void
gnc_split_reg_set_sheet_focus (GNCSplitReg *gsr, gboolean has_focus)
{
    GnucashRegister *reg = gsr->reg;
    GnucashSheet *sheet = gnucash_register_get_sheet (reg);
    gnucash_sheet_set_has_focus (sheet, has_focus);
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

typedef struct
{
    GWeakRef gsr;
    QofBook *book;
} GsrBlankSaveRequest;

static void
gsr_blank_save_finished (SplitRegister *reg, gboolean saved, gpointer user_data)
{
    GsrBlankSaveRequest *request = user_data;
    GObject *object = g_weak_ref_get (&request->gsr);
    GNCSplitReg *gsr = object ? GNC_SPLIT_REG (object) : NULL;

    if (saved && gsr && request->book == gnc_get_current_book () && gsr->ledger &&
        reg == gnc_ledger_display_get_split_register (gsr->ledger))
    {
        gnc_split_register_redraw (reg);
        gnc_split_reg_jump_to_blank (gsr);
    }
    g_clear_object (&gsr);
    g_weak_ref_clear (&request->gsr);
    g_free (request);
}

void
gsr_default_blank_handler( GNCSplitReg *gsr, gpointer data )
{
    SplitRegister *reg;
    GsrBlankSaveRequest *request;

    ENTER("gsr=%p, gpointer=%p", gsr, data);
    reg = gsr && gsr->ledger ?
        gnc_ledger_display_get_split_register (gsr->ledger) : NULL;
    if (!reg)
    {
        LEAVE ("no register");
        return;
    }

    request = g_new0 (GsrBlankSaveRequest, 1);
    request->book = gnc_get_current_book ();
    g_weak_ref_init (&request->gsr, G_OBJECT (gsr));
    gnc_split_register_save_async (reg, TRUE, gsr_blank_save_finished, request);
    LEAVE("save request started");
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
gnc_split_reg_sort (GNCSplitReg *gsr, SortType sort_code, ForceSort fs, Refresh ref)
{
    Query *query = gnc_ledger_display_get_query( gsr->ledger );
    gboolean show_present_divider = FALSE;
    GSList *p1 = NULL, *p2 = NULL, *p3 = NULL, *standard;
    SplitRegister *reg;

    if ((gsr->sort_type == sort_code) && !fs)
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
    if (ref)
        gnc_ledger_display_refresh( gsr->ledger );
}

void
gnc_split_reg_sort_standard_cb(GtkWidget *w, gpointer data)
{
    GNCSplitReg *gsr = data;
    gnc_split_reg_sort(gsr, BY_STANDARD, no_force, refresh);
}

void
gnc_split_reg_sort_date_cb(GtkWidget *w, gpointer data)
{
    GNCSplitReg *gsr = data;
    gnc_split_reg_sort(gsr, BY_DATE, no_force, refresh);
}

void
gnc_split_reg_sort_date_entered_cb(GtkWidget *w, gpointer data)
{
    GNCSplitReg *gsr = data;
    gnc_split_reg_sort(gsr, BY_DATE_ENTERED, no_force, refresh);
}

void
gnc_split_reg_sort_date_reconciled_cb(GtkWidget *w, gpointer data)
{
    GNCSplitReg *gsr = data;
    gnc_split_reg_sort(gsr, BY_DATE_RECONCILED, no_force, refresh);
}

void
gnc_split_reg_sort_num_cb(GtkWidget *w, gpointer data)
{
    GNCSplitReg *gsr = data;
    gnc_split_reg_sort(gsr, BY_NUM, no_force, refresh);
}

void
gnc_split_reg_sort_amount_cb(GtkWidget *w, gpointer data)
{
    GNCSplitReg *gsr = data;
    gnc_split_reg_sort(gsr, BY_AMOUNT, no_force, refresh);
}

void
gnc_split_reg_sort_memo_cb(GtkWidget *w, gpointer data)
{
    GNCSplitReg *gsr = data;
    gnc_split_reg_sort(gsr, BY_MEMO, no_force, refresh);
}

void
gnc_split_reg_sort_desc_cb(GtkWidget *w, gpointer data)
{
    GNCSplitReg *gsr = data;
    gnc_split_reg_sort(gsr, BY_DESC, no_force, refresh);
}

void
gnc_split_reg_sort_action_cb(GtkWidget *w, gpointer data)
{
    GNCSplitReg *gsr = data;
    gnc_split_reg_sort(gsr, BY_ACTION, no_force, refresh);
}

void
gnc_split_reg_sort_notes_cb(GtkWidget *w, gpointer data)
{
    GNCSplitReg *gsr = data;
    gnc_split_reg_sort(gsr, BY_NOTES, no_force, refresh);
}

void
gnc_split_reg_set_sort_reversed(GNCSplitReg *gsr, gboolean rev, Refresh ref)
{
    /* Note: sort_reversed is the boolean opposite of sort_increasing
     *       so when rev == true, we're sorting decreasing
     *       In other words, qof_query_set_sort_increasing should
     *       always use the inverse of rev.
     */
    SplitRegister *reg = gnc_ledger_display_get_split_register (gsr->ledger);
    Query *query = gnc_ledger_display_get_query( gsr->ledger );

    gnc_split_register_set_reverse_sort (reg, rev);

    qof_query_set_sort_increasing (query, !rev, !rev, !rev);
    gsr->sort_rev = rev;

    if (ref)
        gnc_ledger_display_refresh( gsr->ledger );
}

static void gnc_split_reg_goto_next_trans_row (GNCSplitReg *gsr);

typedef struct
{
    GWeakRef gsr;
    QofBook *book;
    gboolean goto_blank;
    gboolean next_transaction;
} GsrEnterSaveRequest;

static void
gsr_enter_save_finished (SplitRegister *reg, gboolean saved, gpointer user_data)
{
    GsrEnterSaveRequest *request = user_data;
    GObject *object = g_weak_ref_get (&request->gsr);
    GNCSplitReg *gsr = object ? GNC_SPLIT_REG (object) : NULL;

    if (saved && gsr && request->book == gnc_get_current_book () && gsr->ledger &&
        reg == gnc_ledger_display_get_split_register (gsr->ledger))
    {
        if (!request->goto_blank && request->next_transaction)
            gnc_split_register_expand_current_trans (reg, FALSE);
        if (request->goto_blank)
            gnc_split_reg_jump_to_blank (gsr);
        else if (request->next_transaction)
            gnc_split_reg_goto_next_trans_row (gsr);
        else
            gnucash_register_goto_next_virt_row (gsr->reg);
    }

    g_clear_object (&gsr);
    g_weak_ref_clear (&request->gsr);
    g_free (request);
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
    SplitRegister *sr;
    gboolean goto_blank;
    GsrEnterSaveRequest *request;

    g_return_if_fail (IS_GNC_SPLIT_REG (gsr));
    sr = gsr->ledger ? gnc_ledger_display_get_split_register (gsr->ledger) : NULL;
    if (!sr)
        return;

    ENTER("gsr=%p, next_transaction=%s", gsr,
          next_transaction ? "TRUE" : "FALSE");
    goto_blank = gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL_REGISTER,
                                     GNC_PREF_ENTER_MOVES_TO_END);
    if (!goto_blank && !next_transaction && sr->style == REG_STYLE_LEDGER)
    {
        Split *blank_split = gnc_split_register_get_blank_split (sr);
        if (blank_split && blank_split == gnc_split_register_get_current_split (sr))
            goto_blank = TRUE;
    }

    request = g_new0 (GsrEnterSaveRequest, 1);
    request->book = gnc_get_current_book ();
    request->goto_blank = goto_blank;
    request->next_transaction = next_transaction;
    g_weak_ref_init (&request->gsr, G_OBJECT (gsr));
    gnc_split_register_save_async (sr, TRUE, gsr_enter_save_finished, request);
    LEAVE ("save request started");
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
    GtkWidget *text_label, *secondary_label;

    hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 2);
    gtk_box_set_homogeneous (GTK_BOX (hbox), FALSE);
    if (pack_start)
        gtk_box_append (GTK_BOX(summarybar), GTK_WIDGET(hbox));
    else
        gtk_box_prepend (GTK_BOX(summarybar), GTK_WIDGET(hbox));
    gtk_box_set_spacing (GTK_BOX(summarybar), 5);

    text_label = gtk_label_new (label_str);
    gnc_label_set_alignment (text_label, 1.0, 0.5 );
    gtk_label_set_ellipsize (GTK_LABEL(text_label), PANGO_ELLIPSIZE_END);
    gtk_box_append (GTK_BOX(hbox), GTK_WIDGET(text_label));

    secondary_label = gtk_label_new ( "" );
    g_object_set_data (G_OBJECT(secondary_label), "text_label", text_label);
    g_object_set_data (G_OBJECT(secondary_label), "text_box", hbox);
    gnc_label_set_alignment (secondary_label, 1.0, 0.5 );
    gtk_box_append (GTK_BOX(hbox), GTK_WIDGET(secondary_label));

    if (extra != NULL)
        gtk_box_append (GTK_BOX(hbox), GTK_WIDGET(extra));

    return secondary_label;
}

static void
gsr_summarybar_set_arrow_icon (GNCSplitReg *gsr)
{
    gtk_image_set_from_icon_name (GTK_IMAGE (gsr->sort_arrow),
                                  gsr->sort_rev ? "pan-down-symbolic" : "pan-up-symbolic");
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
    gsr->sort_arrow = gtk_image_new_from_icon_name ("pan-up-symbolic");
    gtk_image_set_icon_size (GTK_IMAGE(gsr->sort_arrow), GTK_ICON_SIZE_NORMAL);
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
    GWeakRef gsr;
    gchar *string;
} dialog_args;

static void
dialog_args_free (dialog_args *args)
{
    g_weak_ref_clear (&args->gsr);
    g_free (args->string);
    g_free (args);
}

/**
 * Gtk has occasional problems with performing a function as part of a
 * callback. This routine gets called via a timer callback to get it out of
 * the data path with the problem.
 */
static gboolean
gtk_callback_bug_workaround (gpointer argp)
{
    dialog_args *args = argp;
    GObject *owner = g_weak_ref_get (&args->gsr);
    GNCSplitReg *gsr;
    const gchar *read_only_this = _("This account register is read-only.");
    const gchar *read_only_acc = _("The '%s' account register is read-only.");
    gchar *read_only = NULL;
    GNCLedgerDisplayType ledger_type;
    Account *account;
    const gchar *account_name = NULL;
    GtkWindow *parent = NULL;

    if (!owner || !IS_GNC_SPLIT_REG (owner))
    {
        g_clear_object (&owner);
        return G_SOURCE_REMOVE;
    }
    gsr = GNC_SPLIT_REG (owner);
    if (!gsr->ledger)
    {
        g_object_unref (owner);
        return G_SOURCE_REMOVE;
    }

    ledger_type = gnc_ledger_display_type (gsr->ledger);
    account = gnc_ledger_display_leader (gsr->ledger);
    if (account)
    {
        account_name = xaccAccountGetName (account);
        if (ledger_type == LD_SINGLE)
            read_only = g_strdup_printf (read_only_acc, account_name);
        else
        {
            gchar *name = g_strconcat (account_name, "+", NULL);
            read_only = g_strdup_printf (read_only_acc, name);
            g_free (name);
        }
    }
    else
        read_only = g_strdup (read_only_this);

    if (GTK_IS_WINDOW (gsr->window))
        parent = GTK_WINDOW (gsr->window);
    gnc_warning_dialog_async (parent, GNC_PREF_WARN_REG_IS_READ_ONLY,
                              read_only, args->string, _("_Close"),
                              GTK_RESPONSE_CLOSE, TRUE,
                              gsr_warning_dialog_finished, NULL);

    g_free (read_only);
    g_object_unref (owner);
    return G_SOURCE_REMOVE;
}
/**
 * Determines whether this register window should be read-only.
 **/
static
void
gnc_split_reg_determine_read_only( GNCSplitReg *gsr, gboolean show_dialog )
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
        char *string = NULL;
        reg = gnc_ledger_display_get_split_register( gsr->ledger );
        if(reg->mismatched_commodities)
        {
            string = _("The transactions of this account may not be edited "
                       "because its subaccounts have mismatched commodities "
                       "or currencies.\n"
                       "You need to open each account individually to edit "
                       "transactions.");
        }
        else
        {
            switch (gnc_split_reg_get_placeholder(gsr))
            {
            case PLACEHOLDER_NONE:
                /* stay as false. */
                return;

            case PLACEHOLDER_THIS:
                string = _("The transactions of this account may not be edited.\n"
                           "If you want to edit transactions in this register, "
                           "please open the account options and turn off the "
                           "placeholder checkbox.");
                break;

            default:
                string = _("The transactions in one of the selected "
                           "sub-accounts may not be edited.\n"
                           "If you want to edit transactions in this register, please open "
                           "the sub-account options and turn off the placeholder checkbox.\n"
                           "You may also open an individual account instead "
                           "of a set of accounts.");
                break;
            }
        }
        gsr->read_only = TRUE;
        if (show_dialog)
        {
            /* Put up a warning dialog */
            dialog_args *args = g_new0 (dialog_args, 1);
            args->string = g_strdup (string);
            g_weak_ref_init (&args->gsr, G_OBJECT (gsr));

            g_timeout_add_full (G_PRIORITY_DEFAULT, 250,
                                gtk_callback_bug_workaround, args,
                                (GDestroyNotify)dialog_args_free);
        }
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

GtkWidget*
gnc_split_reg_get_summarybar( GNCSplitReg *gsr )
{
    if ( !gsr ) return NULL;
    return gsr->summarybar;
}

gboolean
gnc_split_reg_get_read_only( GNCSplitReg *gsr )
{
    SplitRegister *reg;

    g_assert( gsr );

    // reset read_only flag
    gsr->read_only = FALSE;
    gnc_split_reg_determine_read_only (gsr, FALSE);

    reg = gnc_ledger_display_get_split_register( gsr->ledger );
    gnc_split_register_set_read_only( reg, gsr->read_only );
    return gsr->read_only;
}

void
gnc_split_reg_set_moved_cb( GNCSplitReg *gsr, GFunc cb, gpointer cb_data )
{
    gnucash_register_set_moved_cb (gsr->reg, cb, cb_data);
}
