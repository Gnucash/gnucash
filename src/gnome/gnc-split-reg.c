/********************************************************************\
 * gnc-split-reg.c -- A widget for the common register look-n-feel. *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1997-1998 Linas Vepstas <linas@linas.org>          *
 * Copyright (C) 1998 Rob Browning <rlb@cs.utexas.edu>              *
 * Copyright (C) 1999-2000 Dave Peticolas <dave@krondo.com>         *
 * Copyright (C) 2001 Gnumatic, Inc.                                *
 * Copyright (C) 2002 Joshua Sled <jsled@asynchronous.org>          *
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
\********************************************************************/

#define _GNU_SOURCE

#include "config.h"

#include <gnome.h>
#include <time.h>

#include "gnc-split-reg.h"

#include "window-register.h"
#include "Account.h"
#include "AccWindow.h"
#include "Scrub.h"
#include "dialog-sx-from-trans.h"
#include "global-options.h"
#include "gnc-component-manager.h"
#include "gnc-date-edit.h"
#include "gnc-engine-util.h"
#include "gnc-euro.h"
#include "gnc-gui-query.h"
#include "gnc-ledger-display.h"
#include "gnc-pricedb.h"
#include "gnc-ui-util.h"
#include "gnc-ui.h"
#include "gnucash-sheet.h"
#include "messages.h"
#include "table-allgui.h"

#include <libguile.h>
#include "gnc-engine-util.h"
#include "dialog-utils.h"

static short module = MOD_SX;

/**
 * TODO/FIXME list:
 * . alpha-necessary
 *   X fill out gnc-split-reg.h interface
 *   . calendar/date-picker
 *   . FIXMES
 * . beta-necessary
 *   . date-inclusion on jumping
 *   . title-renaming in read-only case.
 *   . size-allocation
 *   . default schedule/recur handling for from-SX items.
 *   . pass in, use number-of-lines
 *   X handle destruction/cleanup more cleanly
 *   X conditional creation
 *   X handle widget-visibility callbacks
 *   X fix regWindow{Simple,Ledger,Account}
 *   X fix jumping-to-split
 *   X fix window-raising
 **/


/** PROTOTYPES ******************************************************/
void gnc_split_reg_raise( GNCSplitReg *gsr );

static GtkWidget* add_summary_label( GtkWidget *summarybar,
                                     const char *label_str );

static void gnc_toolbar_change_cb( void *data );
static void gnc_split_reg_determine_read_only( GNCSplitReg *gsr );

static void gnc_split_reg_change_style (GNCSplitReg *gsr, SplitRegisterStyle style);

static GNCPlaceholderType gnc_split_reg_get_placeholder( GNCSplitReg *gsr );
static gnc_numeric gsr_account_present_balance( Account *account );
static gncUIWidget gnc_split_reg_get_parent( GNCLedgerDisplay *ledger );

static void gsr_create_menus( GNCSplitReg *gsr );
static void gsr_setup_menu_widgets( GNCSplitReg *gsr, GladeXML *xml );
static void gsr_create_toolbar( GNCSplitReg *gsr );
static void gsr_create_summary_bar( GNCSplitReg *gsr );
static void gsr_create_table( GNCSplitReg *gsr );
static void gsr_setup_table( GNCSplitReg *gsr );
static void gsr_setup_status_widgets( GNCSplitReg *gsr );
static GtkWidget* gsr_create_popup_menu( GNCSplitReg *gsr );


/**
 * Defines a function pointer def to get a gnc_numeric from an account.
 **/
typedef gnc_numeric (*AmountGetterFn)(Account*);

static void gsr_update_summary_label( GtkWidget *label,
                                      AmountGetterFn getter,
                                      Account *leader,
                                      GNCPrintAmountInfo print_info,
                                      gnc_commodity *cmdty,
                                      gboolean reverse,
                                      gboolean euroFlag );

static void gsr_redraw_all_cb (GnucashRegister *g_reg, gpointer data);

static void gnc_split_reg_refresh_toolbar( GNCSplitReg *gsr );

static void gnc_split_reg_ld_destroy( GNCLedgerDisplay *ledger );

gboolean gnc_split_reg_check_close(GNCSplitReg *gsr);

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

static void gsr_emit_signal( GNCSplitReg *gsr, const char *sigName );
static void gsr_emit_help_changed( GnucashRegister *reg, gpointer user_data );


void gnc_split_reg_cut_cb(GtkWidget *w, gpointer data);
void gnc_split_reg_copy_cb(GtkWidget *w, gpointer data);
void gnc_split_reg_paste_cb(GtkWidget *w, gpointer data);

void gnc_split_reg_cut_trans_cb(GtkWidget *w, gpointer data);
void gnc_split_reg_copy_trans_cb(GtkWidget *w, gpointer data);
void gnc_split_reg_paste_trans_cb(GtkWidget *w, gpointer data);

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

gboolean gnc_split_reg_delete_cb( GtkWidget *widget,
                                  GdkEvent *event,
                                  gpointer data );
void gnc_split_reg_destroy_cb(GtkWidget *widget, gpointer data);
void gnc_split_reg_size_allocate( GtkWidget *widget,
                                  GtkAllocation *allocation,
                                  gpointer user_data );

static void gnc_split_reg_class_init( GNCSplitRegClass *class );
static void gnc_split_reg_init( GNCSplitReg *gsr );
static void gnc_split_reg_init2( GNCSplitReg *gsr );

void gnc_split_register_size_allocate (GtkWidget *widget,
                                       GtkAllocation *allocation,
                                       gpointer user_data);


guint
gnc_split_reg_get_type( void )
{
  static guint gnc_split_reg_type = 0;

  if (!gnc_split_reg_type)
    {
      GtkTypeInfo gnc_split_reg_info =
      {
	"GNCSplitReg",
	sizeof (GNCSplitReg),
	sizeof (GNCSplitRegClass),
	(GtkClassInitFunc) gnc_split_reg_class_init,
	(GtkObjectInitFunc) gnc_split_reg_init,
	NULL, /* reserved_1 */
        NULL, /* reserved_2 */
        (GtkClassInitFunc) NULL
      };

      gnc_split_reg_type = gtk_type_unique( GTK_TYPE_VBOX, &gnc_split_reg_info );
    }

  return gnc_split_reg_type;
}

/* SIGNALS */
enum gnc_split_reg_signal_enum {
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
  HELP_CHANGED_SIGNAL,
  LAST_SIGNAL
};

static gint gnc_split_reg_signals[LAST_SIGNAL] = { 0 };

static void
gnc_split_reg_class_init( GNCSplitRegClass *class )
{
  int i;
  GtkObjectClass *object_class;
  static struct similar_signal_info {
    enum gnc_split_reg_signal_enum s;
    const char *signal_name;
    guint defaultOffset;
  } signals[] = {
    { ENTER_ENT_SIGNAL,    "enter_ent",    GTK_SIGNAL_OFFSET( GNCSplitRegClass, enter_ent_cb ) },
    { CANCEL_ENT_SIGNAL,   "cancel_ent",   GTK_SIGNAL_OFFSET( GNCSplitRegClass, cancel_ent_cb ) },
    { DELETE_ENT_SIGNAL,   "delete_ent",   GTK_SIGNAL_OFFSET( GNCSplitRegClass, delete_ent_cb ) },
    { REINIT_ENT_SIGNAL,   "reinit_ent",   GTK_SIGNAL_OFFSET( GNCSplitRegClass, reinit_ent_cb ) },
    { DUP_ENT_SIGNAL,      "dup_ent",      GTK_SIGNAL_OFFSET( GNCSplitRegClass, dup_ent_cb ) },
    { SCHEDULE_ENT_SIGNAL, "schedule_ent", GTK_SIGNAL_OFFSET( GNCSplitRegClass, schedule_ent_cb ) },
    { EXPAND_ENT_SIGNAL,   "expand_ent",   GTK_SIGNAL_OFFSET( GNCSplitRegClass, expand_ent_cb ) },
    { BLANK_SIGNAL,        "blank",        GTK_SIGNAL_OFFSET( GNCSplitRegClass, blank_cb ) },
    { JUMP_SIGNAL,         "jump",         GTK_SIGNAL_OFFSET( GNCSplitRegClass, jump_cb ) },
    { CUT_SIGNAL,          "cut",          GTK_SIGNAL_OFFSET( GNCSplitRegClass, cut_cb ) },
    { CUT_TXN_SIGNAL,      "cut_txn",      GTK_SIGNAL_OFFSET( GNCSplitRegClass, cut_txn_cb ) },
    { COPY_SIGNAL,         "copy",         GTK_SIGNAL_OFFSET( GNCSplitRegClass, copy_cb ) },
    { COPY_TXN_SIGNAL,     "copy_txn",     GTK_SIGNAL_OFFSET( GNCSplitRegClass, copy_txn_cb ) },
    { PASTE_SIGNAL,        "paste",        GTK_SIGNAL_OFFSET( GNCSplitRegClass, paste_cb ) },
    { PASTE_TXN_SIGNAL,    "paste_txn",    GTK_SIGNAL_OFFSET( GNCSplitRegClass, paste_txn_cb ) },
    { HELP_CHANGED_SIGNAL, "help-changed", GTK_SIGNAL_OFFSET( GNCSplitRegClass, help_changed_cb ) },
    { LAST_SIGNAL, NULL, 0 }
  };

  object_class = (GtkObjectClass*) class;

  for ( i=0; signals[i].signal_name != NULL; i++ ) {
    gnc_split_reg_signals[ signals[i].s ] =
      gtk_signal_new( signals[i].signal_name,
                      GTK_RUN_FIRST,
                      object_class->type, signals[i].defaultOffset,
                      gtk_signal_default_marshaller, GTK_TYPE_NONE, 0 );
  }

  gtk_object_class_add_signals (object_class, gnc_split_reg_signals, LAST_SIGNAL);

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

  class->help_changed_cb = NULL;
}

GtkWidget*
gnc_split_reg_new( GNCLedgerDisplay *ld,
                   GtkWindow *parent,
                   gint numberOfLines,
                   gint createFlags,
                   gint disallowCaps )
{
  GNCSplitReg *gsrToRet;

  gsrToRet = GNC_SPLIT_REG( gtk_type_new( gnc_split_reg_get_type() ) );

  gsrToRet->disallowedCaps = disallowCaps;
  gsrToRet->numRows        = numberOfLines;
  gsrToRet->createFlags    = createFlags;

  gsrToRet->ledger = ld;
  gsrToRet->window = GTK_WIDGET(parent);

  gnc_split_reg_init2( gsrToRet );

  return GTK_WIDGET( gsrToRet );
}

static void 
gnc_split_reg_init( GNCSplitReg *gsr )
{
  gsr->sort_type = BY_STANDARD;
  gsr->width = -1;
  gsr->height = -1;
  gsr->disallowedCaps = 0;
  gsr->numRows = (guint) gnc_lookup_number_option ( "_+Advanced",
                                                    "Number of Rows", 20.0 );
  gsr->read_only = FALSE;

  /* IMPORTANT: If we set this to anything other than GTK_RESIZE_QUEUE, we
   * enter into a very bad back-and-forth between the sheet and a containing
   * GnomeDruid [in certain conditions and circumstances not detailed here],
   * resulting in either a single iteration of the Druid resizing or infinite
   * iterations of the Druid resizing without bound.  Contact
   * jsled@asynchronous.org for details. -- 2002.04.15
   */
  gtk_container_set_resize_mode( GTK_CONTAINER(gsr), GTK_RESIZE_QUEUE );

  gtk_signal_connect( GTK_OBJECT(gsr), "destroy",
                      GTK_SIGNAL_FUNC (gnc_split_reg_destroy_cb), gsr );
}

static void 
gnc_split_reg_init2( GNCSplitReg *gsr )
{
  if ( !gsr ) return;

  gnc_split_reg_determine_read_only( gsr );

  if ( gsr->createFlags & CREATE_MENUS ) {
    gsr_create_menus( gsr );
  }

  if ( gsr->createFlags & CREATE_TOOLBAR ) {
    gsr_create_toolbar( gsr );
  }

  if ( gsr->createFlags & CREATE_SUMMARYBAR ) {
    gsr_create_summary_bar( gsr );
  }

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

  if ( gsr->createFlags & CREATE_POPUP ) {
    if ( !gsr->popup_menu ) {
      gsr->popup_menu = gsr_create_popup_menu (gsr);
    }
    gnucash_register_attach_popup( gsr->reg, gsr->popup_menu, gsr );
  }

  sr = gnc_ledger_display_get_split_register( gsr->ledger );
  gnc_split_register_show_present_divider( sr, TRUE );
  gnc_ledger_display_refresh( gsr->ledger );
  gnc_split_reg_refresh_toolbar( gsr );
}


static
void
gsr_create_menus( GNCSplitReg *gsr )
{
  GladeXML *xml;
  GtkWidget *mbar, *mi;
  xml = gnc_glade_xml_new( "register.glade", "register_menubar" );
  glade_xml_signal_autoconnect_full( xml,
                                     gnc_glade_autoconnect_full_func,
                                     gsr );

  mbar = glade_xml_get_widget( xml, "register_menubar" );
  gtk_widget_hide( mbar );

  gsr->edit_menu = glade_xml_get_widget( xml, "menu_edit_menu" );
  gtk_object_ref( GTK_OBJECT(gsr->edit_menu) );
  mi = glade_xml_get_widget( xml, "menu_edit" );
  gtk_menu_item_remove_submenu( GTK_MENU_ITEM( mi ) );

  gsr->view_menu = glade_xml_get_widget( xml, "menu_view_menu" );
  gtk_object_ref( GTK_OBJECT(gsr->view_menu) );
  mi = glade_xml_get_widget( xml, "menu_view" );
  gtk_menu_item_remove_submenu( GTK_MENU_ITEM(mi) );

  gsr->style_submenu = glade_xml_get_widget( xml, "menu_style_menu" );
  gsr->sort_submenu = glade_xml_get_widget( xml, "menu_sort_order_menu" );

  gsr->action_menu = glade_xml_get_widget( xml, "menu_actions_menu" );
  gtk_object_ref( GTK_OBJECT(gsr->action_menu) );
  mi = glade_xml_get_widget( xml, "menu_actions" );
  gtk_menu_item_remove_submenu( GTK_MENU_ITEM(mi) );

  gsr->double_line_check =
    glade_xml_get_widget (xml, "menu_style_double_line");
  gsr->split_menu_check =
    glade_xml_get_widget (xml, "menu_splits");

  gsr_setup_menu_widgets( gsr, xml );

  /* we've ref'd the objects we need to. */
  gtk_widget_destroy( mbar );
}

static
void
gsr_create_toolbar( GNCSplitReg *gsr )
{
  GladeXML *xml;
  GtkWidget *widget;
  SCM id;

  xml = gnc_glade_xml_new( "register.glade", "toolbar" );
  glade_xml_signal_autoconnect_full( xml,
                                     gnc_glade_autoconnect_full_func,
                                     gsr );

  gsr->toolbar = glade_xml_get_widget( xml, "toolbar" );
  gsr->split_button = glade_xml_get_widget( xml, "toolbar_split" );

  id = gnc_register_option_change_callback( gnc_toolbar_change_cb, gsr,
                                            "General", "Toolbar Buttons" );
  gsr->toolbar_change_callback_id = id;


  if ( gsr->disallowedCaps & CAP_DELETE ) {
    widget = glade_xml_get_widget( xml, "toolbar_delete" );
    gtk_widget_set_sensitive( widget, FALSE );
  }

  if ( gsr->disallowedCaps & CAP_JUMP ) {
    widget = glade_xml_get_widget( xml, "toolbar_jump" );
    gtk_widget_set_sensitive( widget, FALSE );
  }

  if ( gsr->disallowedCaps & CAP_SCHEDULE ) {
    widget = glade_xml_get_widget( xml, "toolbar_schedule" );
    gtk_widget_set_sensitive( widget, FALSE );
  }

  if (gsr->read_only) {
    widget = glade_xml_get_widget (xml, "toolbar_delete");
    gtk_widget_set_sensitive(widget, FALSE);
    widget = glade_xml_get_widget (xml, "toolbar_duplicate");
    gtk_widget_set_sensitive(widget, FALSE);
  }
}

static
void
gsr_foobar( GtkWidget *w, gpointer ud )
{
  DEBUG( "Happiness is being destroyed (widget)%.8x, (ud)%.8x",
         w, ud );
}

static
void
gsr_create_table( GNCSplitReg *gsr )
{
  GtkWidget *register_widget;
  SplitRegister *sr;

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

  gtk_container_add( GTK_CONTAINER(gsr), GTK_WIDGET(gsr->reg) );
  GNUCASH_SHEET(gsr->reg->sheet)->window = gsr->window;
  gtk_widget_show_all( GTK_WIDGET(gsr->reg) );
  gtk_signal_connect (GTK_OBJECT(gsr->reg), "activate_cursor",
                      GTK_SIGNAL_FUNC(gnc_split_reg_record_cb), gsr);
  gtk_signal_connect (GTK_OBJECT(gsr->reg), "redraw_all",
                      GTK_SIGNAL_FUNC(gsr_redraw_all_cb), gsr);
  gtk_signal_connect (GTK_OBJECT(gsr->reg), "redraw_help",
                      GTK_SIGNAL_FUNC(gsr_emit_help_changed), gsr);


  gtk_signal_connect (GTK_OBJECT(gsr->reg), "destroy",
                      GTK_SIGNAL_FUNC(gsr_foobar), gsr);
}

static
void
gsr_setup_status_widgets( GNCSplitReg *gsr )
{
  SplitRegister *sr;
  gboolean use_double_line;
  GtkCheckMenuItem *check;

  sr = gnc_ledger_display_get_split_register( gsr->ledger );
  use_double_line = gnc_ledger_display_default_double_line( gsr->ledger );

  /* be sure to initialize the gui elements associated with the cursor */
  gnc_split_register_config( sr, sr->type, sr->style, use_double_line );

  check = GTK_CHECK_MENU_ITEM( gsr->double_line_check );

  gtk_signal_handler_block_by_func
    ( GTK_OBJECT(check),
      GTK_SIGNAL_FUNC(gnc_split_reg_double_line_cb), gsr );

  gtk_check_menu_item_set_active(check, use_double_line);

  gtk_signal_handler_unblock_by_func
    ( GTK_OBJECT(check),
      GTK_SIGNAL_FUNC(gnc_split_reg_double_line_cb), gsr );
}

void
gnc_split_reg_destroy_cb(GtkWidget *widget, gpointer data)
{
  GNCSplitReg *gsr = data;
  SCM id;

  id = gsr->toolbar_change_callback_id;
  gnc_unregister_option_change_callback_id(id);
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
                          AmountGetterFn getter,
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

  if ( reverse ) {
    amount = gnc_numeric_neg( amount );
  }

  xaccSPrintAmount( string, amount, print_info );

  if ( euroFlag ) {
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
  GNCBook *book;
  GNCPriceDB *pdb;
  gnc_commodity *commodity;
  gnc_commodity *currency;

  commodity = xaccAccountGetCommodity (account);
  currency = gnc_default_currency ();

  book = gnc_get_current_book ();
  pdb = gnc_book_get_pricedb (book);

  return gnc_pricedb_lookup_latest (pdb, commodity, currency);
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

  if ( gsr->window == NULL )
    return;

  leader = gnc_ledger_display_leader( gsr->ledger );
  euro = gnc_lookup_boolean_option( "International",
                                    "Enable EURO support",
                                    FALSE );

  commodity = xaccAccountGetCommodity( leader );

  /* no EURO converson, if account is already EURO or no EURO currency */
  if (commodity != NULL)
    euro = (euro && gnc_is_euro_currency( commodity ));
  else
    euro = FALSE;

  print_info = gnc_account_print_info( leader, TRUE );
  reverse = gnc_reverse_balance( leader );

  if ( gsr->createFlags & CREATE_SUMMARYBAR ) {
    gsr_update_summary_label( gsr->balance_label,
                              (AmountGetterFn)gsr_account_present_balance,
                              leader, print_info, commodity, reverse, euro );
    gsr_update_summary_label( gsr->cleared_label,
                              (AmountGetterFn)xaccAccountGetClearedBalance,
                              leader, print_info, commodity, reverse, euro );
    gsr_update_summary_label( gsr->reconciled_label,
                              (AmountGetterFn)xaccAccountGetReconciledBalance,
                              leader, print_info, commodity, reverse, euro );
    gsr_update_summary_label( gsr->future_label,
                              (AmountGetterFn)xaccAccountGetBalance,
                              leader, print_info, commodity, reverse, euro );

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

    if (gsr->value_label != NULL)
      {
        GNCPrice *price;

        price = account_latest_price (leader);
        if (!price)
          {
            gnc_set_label_color (gsr->value_label, gnc_numeric_zero ());
            gtk_label_set_text (GTK_LABEL (gsr->value_label),
                                _("<No information>"));
          }
        else
          {
            gnc_commodity *currency = gnc_price_get_currency (price);

            print_info = gnc_commodity_print_info (currency, TRUE);

            amount = xaccAccountGetBalance (leader);
            if (reverse)
              amount = gnc_numeric_neg (amount);

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

  /* FIXME */
#if 0 /* FIXME */
  gnc_reg_set_window_name( gsr );
#endif /* 0 -- FIXME */

  {
    gboolean expand;
    gboolean sensitive;
    SplitRegister *reg;

    reg = gnc_ledger_display_get_split_register( gsr->ledger );
    expand = gnc_split_register_current_trans_expanded (reg);
    sensitive = (reg->style == REG_STYLE_LEDGER);

    if ( gsr->createFlags & CREATE_TOOLBAR ) {
      gtk_signal_handler_block_by_data
        (GTK_OBJECT(gsr->split_button), gsr);
      gtk_toggle_button_set_active
        (GTK_TOGGLE_BUTTON (gsr->split_button), expand);
      gtk_signal_handler_unblock_by_data
        (GTK_OBJECT (gsr->split_button), gsr);
      gtk_widget_set_sensitive( gsr->split_button, sensitive );
    }

    if ( gsr->createFlags & CREATE_MENUS ) {
      gtk_signal_handler_block_by_data
        (GTK_OBJECT (gsr->split_menu_check), gsr);
      gtk_check_menu_item_set_active
        (GTK_CHECK_MENU_ITEM (gsr->split_menu_check), expand);
      gtk_signal_handler_unblock_by_data
        (GTK_OBJECT (gsr->split_menu_check), gsr);
      gtk_widget_set_sensitive( gsr->split_menu_check, sensitive );
    }

    if ( gsr->createFlags & CREATE_POPUP ) {
      gtk_signal_handler_block_by_data
        (GTK_OBJECT (gsr->split_popup_check), gsr);
      gtk_check_menu_item_set_active
        (GTK_CHECK_MENU_ITEM (gsr->split_popup_check), expand);
      gtk_signal_handler_unblock_by_data
        (GTK_OBJECT (gsr->split_popup_check), gsr);
      gtk_widget_set_sensitive( gsr->split_popup_check, sensitive );
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

    gtk_widget_hide_all( gsr->window );
    DEBUG( "destroying (gsr)%.8x (->window)%.8x with ledger %.8x",
           gsr, gsr->window, ledger );
    gtk_widget_destroy( gsr->window );
  }
  gnc_ledger_display_set_user_data (ledger, NULL);
}

gboolean
gnc_split_reg_check_close( GNCSplitReg *gsr )
{
  gboolean pending_changes;
  SplitRegister *reg;

  reg = gnc_ledger_display_get_split_register( gsr->ledger );
  pending_changes = gnc_split_register_changed( reg );
  if ( !pending_changes )
    return FALSE;

  {
    const char *message = _("The current transaction has been changed.\n"
                            "Would you like to record it?");
    if ( gnc_verify_dialog_parented( gsr->window, TRUE, message) ) {
      gnc_split_reg_record_trans_cb( gsr->window, gsr );
      return TRUE;
    } else {
      gnc_split_register_cancel_cursor_trans_changes( reg );
      return FALSE;
    }
  }
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
  gsr_emit_signal( gsr, "cut" );
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
  gsr_emit_signal( gsr, "copy" );
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
  gsr_emit_signal( gsr, "paste" );
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
  gsr_emit_signal( gsr, "cut_txn" );
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
  gsr_emit_signal( gsr, "copy_txn" );
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
  gsr_emit_signal( gsr, "paste_txn" );
}

/* Remove when porting to gtk2.0 */
#define GTK_STOCK_CANCEL           GNOME_STOCK_BUTTON_CANCEL
#define GTK_STOCK_DELETE           "Delete"

void
gsr_default_reinit_handler( GNCSplitReg *gsr, gpointer data )
{
  VirtualCellLocation vcell_loc;
  SplitRegister *reg;
  Transaction *trans;
  Split *split;
  char *buf = NULL;
  gint result;
  const char *two_choices[] = { N_(GTK_STOCK_CANCEL),
				N_("Remove Transaction Splits"),
				NULL };
  const char *message = _("Are you sure you want to remove the "
			  "Splits of this transaction?");

  const char *recn_warn = _("You would be modifying a "
			    "transaction with reconciled splits!\n"
			    "This is not a good idea as it will cause your "
			    "reconciled balance to be off.");

  reg = gnc_ledger_display_get_split_register( gsr->ledger );

  trans = gnc_split_register_get_current_trans (reg);
  if (xaccTransWarnReadOnly(trans))
    return;
  if (xaccTransHasReconciledSplits (trans)) {
    buf = g_strconcat (message, "\n\n", recn_warn, NULL);
    result =
      gnc_generic_warning_dialog_parented(gsr->window, two_choices, buf);
  } else {
      buf = g_strdup (message);
      result =
	gnc_generic_question_dialog_parented(gsr->window, two_choices,buf);
  }
  g_free(buf);
  if (!result)
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
  gsr_emit_signal( gsr, "reinit_ent" );
}

void
gsr_default_delete_handler( GNCSplitReg *gsr, gpointer data )
{
  SplitRegisterStyle style;
  CursorClass cursor_class;
  SplitRegister *reg;
  Transaction *trans;
  char *buf = NULL;
  Split *split;
  gint result;
  const char *two_choices[] = { N_(GTK_STOCK_CANCEL),
				N_(GTK_STOCK_DELETE),
				NULL };

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
    const char *format = _("Are you sure you want to delete\n   %s\n"
                           "from the transaction\n   %s ?");
    const char *recn_warn = _("You would be deleting a reconciled split!\n"
			      "This is not a good idea as it will cause your "
			      "reconciled balance to be off.");
    const char *memo;
    const char *desc;
    char recn;

    memo = xaccSplitGetMemo (split);
    memo = (memo && *memo) ? memo : _("(no memo)");

    desc = xaccTransGetDescription (trans);
    desc = (desc && *desc) ? desc : _("(no description)");

    /* ask for user confirmation before performing permanent damage */
    buf = g_strdup_printf (format, memo, desc);

    recn = xaccSplitGetReconcile (split);
    if (recn == YREC || recn == FREC)
    {
      char *new_buf;

      new_buf = g_strconcat (buf, "\n\n", recn_warn, NULL);
      g_free (buf);
      buf = new_buf;
      result =
	gnc_generic_warning_dialog_parented(gsr->window, two_choices, buf);
    } else {
      result =
	gnc_generic_question_dialog_parented(gsr->window, two_choices,buf);
    }
    g_free(buf);

    if (!result)
      return;

    gnc_split_register_delete_current_split (reg);
    return;
  }

  g_return_if_fail(cursor_class == CURSOR_CLASS_TRANS);

  /* On a transaction cursor with 2 or fewer splits in single or double
   * mode, we just delete the whole transaction, kerblooie */
  {
    const char *message = _("Are you sure you want to delete the current "
                            "transaction?");
    const char *recn_warn = _("You would be deleting a transaction "
                              "with reconciled splits!\n"
			      "This is not a good idea as it will cause your "
			      "reconciled balance to be off.");
    char *buf;

    if (xaccTransHasReconciledSplits (trans)) {
      buf = g_strconcat (message, "\n\n", recn_warn, NULL);
      result =
	gnc_generic_warning_dialog_parented(gsr->window, two_choices, buf);
    } else {
      buf = g_strdup (message);
      result =
	gnc_generic_question_dialog_parented(gsr->window, two_choices,buf);
    }

    g_free (buf);

    if (!result)
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
  gsr_emit_signal( gsr, "delete_ent" );
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
  gsr_emit_signal( gsr, "dup_ent" );
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

  /* FIXME: If the transaction has a sched-xact KVP frame, then go to the
   * editor for the existing SX; otherwise, do the sx-from-trans dialog. */
  {
    kvp_frame *txn_frame;
    kvp_value *kvp_val;
    /* set a kvp-frame element in the transaction indicating and
     * pointing-to the SX this was created from. */
    txn_frame = xaccTransGetSlots( pending_trans );
    if ( txn_frame != NULL ) {
      DEBUG( "Got frame, looking up key" );
      kvp_val = kvp_frame_get_slot( txn_frame, "from-sched-xaction" );
      if ( kvp_val ) {
        DEBUG( "Find SX with GUID \"%s\"",
               guid_to_string( kvp_value_get_guid( kvp_val ) ) );
      }
    }
  }

  gnc_sx_create_from_trans(pending_trans);
}

void
gnc_split_reg_recur_cb(GtkWidget *w, gpointer data)
{
  GNCSplitReg *gsr = data;
  gsr_emit_signal( gsr, "schedule_ent" );
}

/**
 * Records into the books the currently-selected transaction.
 **/
void
gnc_split_reg_record_trans_cb (GtkWidget *w, gpointer data)
{
  GNCSplitReg *gsr = data;
  gsr_emit_signal( gsr, "enter_ent" );
}

/* typedef enum */
/* { */
/*   DELETE_CANCEL, */
/*   DELETE_SPLITS, */
/*   DELETE_TRANS, */
/* } DeleteType; */

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
  gsr_emit_signal( gsr, "cancel_ent" );
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
  gsr_emit_signal( gsr, "expand_ent" );
}

void
gnc_split_reg_expand_trans_toolbar_cb (GtkWidget *widget, gpointer data)
{
  GNCSplitReg *gsr = data;
  gsr_emit_signal( gsr, "expand_ent" );
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
#if 0 /* FIXME */
  if (trans != NULL)
    if (gnc_register_include_date(gsr, xaccTransGetDate(trans)))
    {
      gnc_ledger_display_refresh( gsr->ledger );
    }
#endif /* 0 -- FIXME */

  reg = gnc_ledger_display_get_split_register( gsr->ledger );

  if (gnc_split_register_get_split_virt_loc(reg, split, &vcell_loc))
    gnucash_register_goto_virt_cell( gsr->reg, vcell_loc );
}


/**
 * Move the cursor to the split in the non-blank amount column.
 **/
void
gnc_split_reg_jump_to_split_amount(GNCSplitReg *gsr, Split *split)
{
  VirtualLocation virt_loc;
  SplitRegister *reg;

  if (!gsr) return;

#if 0 /* FIXME */
  trans = xaccSplitGetParent(split);
  if (trans != NULL) {
    if (gnc_register_include_date(gsr, xaccTransGetDate(trans)))
    {
      gnc_ledger_display_refresh (gsr->ledger);
    }
  }
#endif /* 0 -- FIXME */

  reg = gnc_ledger_display_get_split_register (gsr->ledger);

  if (gnc_split_register_get_split_amount_virt_loc (reg, split, &virt_loc))
    gnucash_register_goto_virt_loc (gsr->reg, virt_loc);
}

void
gnc_split_reg_jump_to_blank (GNCSplitReg *gsr)
{
  SplitRegister *reg = gnc_ledger_display_get_split_register (gsr->ledger);
  VirtualCellLocation vcell_loc;
  Split *blank;

  blank = gnc_split_register_get_blank_split (reg);
  if (blank == NULL)
    return;

  if (gnc_split_register_get_split_virt_loc (reg, blank, &vcell_loc))
    gnucash_register_goto_virt_cell (gsr->reg, vcell_loc);
}

void
gsr_default_blank_handler( GNCSplitReg *gsr, gpointer data )
{
  SplitRegister *reg;

  reg = gnc_ledger_display_get_split_register (gsr->ledger);

  if (gnc_split_register_save (reg, TRUE))
    gnc_split_register_redraw (reg);

  gnc_split_reg_jump_to_blank (gsr);
}

void
gnc_split_reg_new_trans_cb (GtkWidget *widget, gpointer data)
{
  GNCSplitReg *gsr = data;
  gsr_emit_signal( gsr, "blank" );
}

void
gsr_default_jump_handler( GNCSplitReg *gsr, gpointer data )
{
  SplitRegister *reg;
  Account *account;
  Account *leader;
  Split *split;

  reg = gnc_ledger_display_get_split_register (gsr->ledger);

  split = gnc_split_register_get_current_split (reg);
  if (split == NULL)
    return;

  account = xaccSplitGetAccount(split);
  if (account == NULL)
    return;

  leader = gnc_ledger_display_leader( gsr->ledger );

  if (account == leader)
  {
    split = xaccSplitGetOtherSplit(split);
    if (split == NULL)
      return;

    account = xaccSplitGetAccount(split);
    if (account == NULL)
      return;
    if (account == leader)
      return;
  }

  {
    GNCLedgerDisplay *ld;
    GNCSplitReg *gsr;

    ld = gnc_ledger_display_simple( account );
    gsr = gnc_ledger_display_get_user_data( ld );
    if ( !gsr ) {
      /* create new */
      RegWindow *rw = regWindowSimple( account );
      gnc_register_raise( rw );
      gnc_register_jump_to_split( rw, split );
    } else {
      /* Use existing. */
      gtk_window_present( GTK_WINDOW(gsr->window) );
      gnc_split_reg_jump_to_split( gsr, split );
    }
  }
}

void
gnc_split_reg_jump_cb( GtkWidget *widget, gpointer data )
{
  GNCSplitReg *gsr = data;
  gsr_emit_signal( gsr, "jump" );
}

static
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

static void
gnc_split_reg_record (GNCSplitReg *gsr)
{
  SplitRegister *reg;
  Transaction *trans;

  reg = gnc_ledger_display_get_split_register (gsr->ledger);
  trans = gnc_split_register_get_current_trans (reg);

  if (!gnc_split_register_save (reg, TRUE))
    return;

  /* FIXME */
#if 0 /* FIXME */
  if (trans != NULL)
    gnc_split_reg_include_date( gsr, xaccTransGetDate(trans) );
#endif /* 0 - FIXME */

  gnc_split_register_redraw (reg);
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
  gnucash_register_goto_next_matching_row( gsr->reg,
                                           gnc_split_reg_match_trans_row,
                                           gsr );
}

static void
gnc_split_reg_enter( GNCSplitReg *gsr, gboolean next_transaction )
{
  SplitRegister *sr = gnc_ledger_display_get_split_register( gsr->ledger );
  gboolean goto_blank;

  goto_blank = gnc_lookup_boolean_option( "Register",
                                          "'Enter' moves to blank transaction",
                                          FALSE );

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
}

void
gsr_default_enter_handler( GNCSplitReg *gsr, gpointer data )
{
  gnc_split_reg_enter( gsr, FALSE );
}

void
gnc_split_reg_record_cb (GnucashRegister *reg, gpointer data)
{
  gsr_emit_signal( (GNCSplitReg*)data, "enter_ent" );
}

gboolean
gnc_split_reg_delete_cb(GtkWidget *widget, GdkEvent *event, gpointer data)
{
  GNCSplitReg *gsr = data;
  gnc_split_reg_check_close( gsr );
  gnc_ledger_display_close( gsr->ledger );
  return TRUE; /* don't close */
}

void
gnc_split_reg_size_allocate (GtkWidget *widget,
                             GtkAllocation *allocation,
                             gpointer user_data)
{
  GNCSplitReg *gsr = user_data;
  gsr->width = allocation->width;
  /* FIXME: this can't be correct... */
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

static
void
gsr_create_summary_bar( GNCSplitReg *gsr )
{
  gboolean has_shares;
  GtkWidget *summarybar;

  gsr->cleared_label    = NULL;
  gsr->balance_label    = NULL;
  gsr->reconciled_label = NULL;
  gsr->future_label     = NULL;
  gsr->shares_label     = NULL;
  gsr->value_label      = NULL;

  if ( gnc_ledger_display_type(gsr->ledger) >= LD_SUBACCOUNT ) {
    gsr->summarybar = NULL;
  }

  {
    Account *account;
    GNCAccountType atype;

    account = gnc_ledger_display_leader( gsr->ledger );
    atype = xaccAccountGetType (account);

    switch (atype)
    {
      case STOCK:
      case MUTUAL:
      case CURRENCY:
        has_shares = TRUE;
        break;

      default:
        has_shares = FALSE;
        break;
    }
  }

  summarybar = gtk_hbox_new (FALSE, 4);

  if (!has_shares)
  {
    gsr->balance_label    = add_summary_label (summarybar, _("Present:"));
    gsr->future_label     = add_summary_label (summarybar, _("Future:"));
    gsr->cleared_label    = add_summary_label (summarybar, _("Cleared:"));
    gsr->reconciled_label = add_summary_label (summarybar, _("Reconciled:"));
  }
  else
  {
    gsr->shares_label     = add_summary_label (summarybar, _("Shares:"));
    gsr->value_label      = add_summary_label (summarybar, _("Current Value:"));
  }

  gsr->summarybar = summarybar;
}

static
void
gsr_setup_menu_widgets(GNCSplitReg *gsr, GladeXML *xml)
{
  /* Make sure the right style radio item is active */
  SplitRegister *reg;
  GtkWidget *widget;
  char *widget_name;

  if ( gsr->disallowedCaps & CAP_DELETE ) {
    widget = glade_xml_get_widget( xml, "menu_delete" );
    gtk_widget_set_sensitive( widget, FALSE );
  }

  if ( gsr->disallowedCaps & CAP_JUMP ) {
    widget = glade_xml_get_widget( xml, "menu_jump" );
    gtk_widget_set_sensitive( widget, FALSE );
  }

  if ( gsr->disallowedCaps & CAP_SCHEDULE ) {
    widget = glade_xml_get_widget( xml, "menu_schedule" );
    gtk_widget_set_sensitive( widget, FALSE );
  }
    

  if (gsr->read_only) {
    widget = glade_xml_get_widget (xml, "menu_paste");
    gtk_widget_set_sensitive(widget, FALSE);
    widget = glade_xml_get_widget (xml, "menu_cut_trans");
    gtk_widget_set_sensitive(widget, FALSE);
    widget = glade_xml_get_widget (xml, "menu_paste_trans");
    gtk_widget_set_sensitive(widget, FALSE);
    widget = glade_xml_get_widget (xml, "menu_delete");
    gtk_widget_set_sensitive(widget, FALSE);
    widget = glade_xml_get_widget (xml, "menu_duplicate");
    gtk_widget_set_sensitive(widget, FALSE);
    widget = glade_xml_get_widget (xml, "menu_reinitialize");
    gtk_widget_set_sensitive(widget, FALSE);
  }

  reg = gnc_ledger_display_get_split_register( gsr->ledger );

  switch (reg->style)
    {
    default:
    case REG_STYLE_LEDGER:
      widget_name = "menu_style_basic_ledger";
      break;
    case REG_STYLE_AUTO_LEDGER:
      widget_name = "menu_style_auto_split_ledger";
      break;
    case REG_STYLE_JOURNAL:
      widget_name = "menu_style_transaction_journal";
      break;
    }

  /* registers with more than one account can only use journal mode */
  if (reg->type >= NUM_SINGLE_REGISTER_TYPES)
    {
      widget = glade_xml_get_widget(xml, "menu_style_basic_ledger");
      gtk_widget_set_sensitive (widget, FALSE);

      widget = glade_xml_get_widget(xml, "menu_style_auto_split_ledger");
      gtk_widget_set_sensitive (widget, FALSE);
    }

  widget = glade_xml_get_widget(xml, widget_name);
  gtk_signal_handler_block_by_data(GTK_OBJECT(widget), gsr);
  gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(widget), TRUE);
  gtk_signal_handler_unblock_by_data(GTK_OBJECT(widget), gsr);
}

static
GtkWidget *
gsr_create_popup_menu( GNCSplitReg *gsr )
{
  GtkWidget *popup, *menuitem;
  GladeXML *xml;

  xml = gnc_glade_xml_new( "register.glade", "register_popup" );
  popup = glade_xml_get_widget( xml, "register_popup" );
  glade_xml_signal_autoconnect_full( xml,
                                     gnc_glade_autoconnect_full_func,
				     gsr );

  /* Glade insists on making this a tearoff menu. */
  if (gnome_preferences_get_menus_have_tearoff()) {
    GtkMenuShell *ms = GTK_MENU_SHELL(popup);
    GtkWidget *tearoff;

    tearoff = g_list_nth_data(ms->children, 0);
    ms->children = g_list_remove(ms->children, tearoff);
    gtk_widget_destroy(tearoff);
  }

  gsr->split_popup_check = glade_xml_get_widget( xml, "popup_splits" );

  if ( gsr->disallowedCaps & CAP_DELETE ) {
    menuitem = glade_xml_get_widget( xml, "sr_popup_delete" );
    gtk_widget_set_sensitive( menuitem, FALSE );
  }

  if ( gsr->disallowedCaps & CAP_JUMP ) {
    menuitem = glade_xml_get_widget( xml, "sr_popup_jump" );
    gtk_widget_set_sensitive( menuitem, FALSE );
  }

  if ( gsr->disallowedCaps & CAP_SCHEDULE ) {
    menuitem = glade_xml_get_widget( xml, "sr_popup_schedule" );
    gtk_widget_set_sensitive( menuitem, FALSE );
  }

  if (gsr->read_only) {
    menuitem = glade_xml_get_widget( xml, "sr_popup_delete" );
    gtk_widget_set_sensitive( menuitem, FALSE );
    menuitem = glade_xml_get_widget( xml, "sr_popup_duplicate" );
    gtk_widget_set_sensitive( menuitem, FALSE );
    menuitem = glade_xml_get_widget( xml, "sr_popup_reinitialize" );
    gtk_widget_set_sensitive( menuitem, FALSE );
  }

  return popup;
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
  if (single_account) {
    if (xaccAccountGetPlaceholder( leader ))
      return PLACEHOLDER_THIS;
    return PLACEHOLDER_NONE;
  }
  return xaccAccountGetDescendantPlaceholder( leader );
}

/**
 * @see gtk_callback_bug_workaround
 **/
typedef struct dialog_args  {
  GNCSplitReg *gsr;
  gchar *string;
} dialog_args;

/**
 * Gtk has occasional problems with performing function as part of a
 * callback.  This routine gets called via a timer callback to get it out of
 * the data path with the problem.
 **/
static
gint
gtk_callback_bug_workaround (gpointer argp)
{
  dialog_args *args = argp;

  gnc_warning_dialog_parented(args->gsr->window, args->string);
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
  gchar *old_title, *new_title;
  SplitRegister *reg;
  GtkArg objarg;

  gsr->read_only = FALSE;

  if ( gsr->disallowedCaps & CAP_READ_ONLY ) {

    /* FIXME: this is not ideal, as whatever window-title solution we come up
     * with should be used in this case as well. */

    gsr->read_only = TRUE;

  } else {

    switch (gnc_split_reg_get_placeholder(gsr)) {
    case PLACEHOLDER_NONE:
      /* stay as false. */
      return;

    case PLACEHOLDER_THIS:
      args->string = _("This account may not be edited.  If you want\n"
                       "to edit transactions in this register, please\n"
                       "open the account options and turn off the\n"
                       "placeholder checkbox.");
      break;

    default:
      args->string = _("One of the sub-accounts selected may not be\n"
                       "edited.  If you want to edit transactions in\n"
                       "this register, please open the sub-account\n"
                       "options and turn off the placeholder checkbox.\n"
                       "You may also open an individual account instead\n"
                       "of a set of accounts.");
      break;
    }
    gsr->read_only = TRUE;
    /* Put up a warning dialog */
    args->gsr = gsr;
    gtk_timeout_add (250, gtk_callback_bug_workaround, args); /* 0.25 seconds */
  }

  /* Make the contents immutable */
  reg = gnc_ledger_display_get_split_register( gsr->ledger );
  gnc_split_register_set_read_only( reg, TRUE );

  /* Rename the window title */
  /* FIXME: This isn't so good ... this thing shouldn't be directing
   * window-title changes ... especially for the SX-related stuff. */
  objarg.name = "GtkWindow::title";
  gtk_object_arg_get(GTK_OBJECT(gsr->window), &objarg, NULL);
  old_title = GTK_VALUE_STRING(objarg);
  new_title = g_strdup_printf(_("%s [Read-Only]"), old_title);
  /*gtk_object_set(GTK_OBJECT(gsr->window),
   *               "GtkWindow::title", new_title, NULL); */
  gtk_window_set_title( GTK_WINDOW(gsr->window), new_title );
  g_free(old_title);
  g_free(new_title);
}

static
void
gnc_toolbar_change_cb (void *data)
{
  GNCSplitReg *gsr = data;
  gnc_split_reg_refresh_toolbar( gsr );
}

/**
 * A utility function which retreives the present balance from an Account.
 * This should move somewhere more general?
 **/
static
gnc_numeric
gsr_account_present_balance (Account *account)
{
  GList *list;
  GList *node;
  time_t today;
  struct tm *tm;

  if (!account)
    return gnc_numeric_zero ();

  today = time (NULL);
  tm = localtime (&today);
  tm->tm_hour = 23;
  tm->tm_min = 59;
  tm->tm_sec = 59;
  tm->tm_isdst = -1;
  today = mktime (tm);

  list = xaccAccountGetSplitList (account);
  for (node = g_list_last (list); node; node = node->prev)
  {
    Split *split = node->data;

    if (xaccTransGetDate (xaccSplitGetParent (split)) <= today)
      return xaccSplitGetBalance (split);
  }

  return gnc_numeric_zero ();
}

static
gncUIWidget
gnc_split_reg_get_parent( GNCLedgerDisplay *ledger )
{
  GNCSplitReg *gsr =
    GNC_SPLIT_REG(gnc_ledger_display_get_user_data( ledger ));

  if (gsr == NULL)
    return NULL;

  DEBUG( "(ledger)%.8x parent: (gsr->window)%.8x",
         ledger, gsr->window );

  return gsr->window;
}

static
void
gsr_emit_help_changed( GnucashRegister *reg, gpointer user_data )
{
  gsr_emit_signal( (GNCSplitReg*)user_data, "help-changed" );
}

static
void
gsr_emit_signal( GNCSplitReg *gsr, const char *sigName )
{
  gtk_signal_emit_by_name( GTK_OBJECT(gsr), sigName, NULL );
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
  PERR( "unimplemented" );
}

GtkWidget*
gnc_split_reg_get_edit_menu( GNCSplitReg *gsr )
{
  if ( !gsr ) return NULL;
  return gsr->edit_menu;
}

GtkWidget*
gnc_split_reg_get_view_menu( GNCSplitReg *gsr )
{
  if ( !gsr ) return NULL;
  return gsr->view_menu;
}

GtkWidget*
gnc_split_reg_get_style_menu( GNCSplitReg *gsr )
{
  if ( !gsr ) return NULL;
  return gsr->style_submenu;
}

GtkWidget*
gnc_split_reg_get_sort_menu( GNCSplitReg *gsr )
{
  if ( !gsr ) return NULL;
  return gsr->sort_submenu;
}

GtkWidget*
gnc_split_reg_get_action_menu( GNCSplitReg *gsr )
{
  if ( !gsr ) return NULL;
  return gsr->action_menu;
}

GtkWidget*
gnc_split_reg_get_toolbar( GNCSplitReg *gsr )
{
  if ( !gsr ) return NULL;
  return gsr->toolbar;
}

GtkWidget*
gnc_split_reg_get_summarybar( GNCSplitReg *gsr )
{
  if ( !gsr ) return NULL;
  return gsr->summarybar;
}

GtkWidget*
gnc_split_reg_get_popup( GNCSplitReg *gsr )
{
  if ( !gsr ) return NULL;
  return gsr->popup_menu;
}

void
gnc_split_reg_set_split_state( GNCSplitReg *gsr, gboolean split )
{
  g_assert( gsr );
  PERR( "Unimplemented" );
}

void
gnc_split_reg_set_double_line( GNCSplitReg *gsr, gboolean doubleLine )
{
  g_assert( gsr );
  PERR( "unimplemented" );
}

GtkWidget*
gnc_split_reg_get_popup_extended( GNCSplitReg *gsr )
{
  g_assert( gsr );
  PERR( "unimplemented" );
  return NULL;
}
