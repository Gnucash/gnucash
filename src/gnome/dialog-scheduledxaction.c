/********************************************************************\
 * dialog-scheduledxaction.c : dialog for scheduled transaction     *
 *    list and editor                                               *
 * Copyright (C) 2001 Joshua Sled <jsled@asynchronous.org>          *
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

#include "config.h"

#include <gnome.h>
#include <locale.h>
#include <time.h>

#include "FreqSpec.h"
#include "SchedXaction.h"
#include "dialog-scheduledxaction.h"
#include "dialog-utils.h"
#include "gnc-book.h"
#include "gnc-component-manager.h"
#include "gnc-date-edit.h"
#include "gnc-engine-util.h"
#include "gnc-frequency.h"
#include "gnc-gui-query.h"
#include "gnc-ledger-display.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "global-options.h"
#include "gnucash-sheet.h"
#include "messages.h"
#include "window-help.h"
#include "window-register.h"

static short module = MOD_SX;

#define DIALOG_SCHEDXACTION_CM_CLASS "dialog-scheduledtransactions"
#define DIALOG_SCHEDXACTION_EDITOR_CM_CLASS "dialog-scheduledtransaction-editor"

#define SX_OPT_STR "Scheduled Transactions"
#define AUTOCREATE_OPT "autocreate_opt"
#define NOTIFY_OPT "notify_opt"
#define ADVANCE_OPT "advance_opt"
#define ADVANCE_DAYS_SPIN "advance_days"
#define REMIND_OPT "remind_opt"
#define REMIND_DAYS_SPIN "remind_days"

#define END_OPTION 0
#define NUM_OCCUR_OPTION 1

/** Datatypes ***********************************************************/

/* FIXME: this is stolen from window-register.c */
typedef enum
{
        DELETE_TRANS,
        DELETE_SPLITS,
        DELETE_CANCEL
} DeleteType;

typedef enum _EndTypeEnum {
        END_NONE,
        END_DATE,
        END_OCCUR,
} EndType;

struct _SchedXactionDialog
{
        GtkWidget        *dialog;
        GladeXML        *gxml;

       /* other pertinant scheduled-transaction-editor info */
};

struct _SchedXactionEditorDialog
{
        GladeXML                *gxml;
        GtkWidget                 *dialog;
        SchedXactionDialog         *sxd;
        SchedXaction                 *sx;
        int new;

        GNCLedgerDisplay         *ledger;
        GnucashRegister         *reg;

        GNCFrequency                 *gncfreq;

        char                        *sxGUIDstr;

        GtkWidget *toolbar;
};

/** Prototypes **********************************************************/

static void putSchedXactionInClist( gpointer data, gpointer user_data );

static void schedXact_populate( SchedXactionDialog * );
static void schedXact_editor_init( SchedXactionEditorDialog * );
static void schedXact_editor_create_freq_sel( SchedXactionEditorDialog *sxed );
static void schedXact_editor_create_ledger( SchedXactionEditorDialog *sxed );
static void schedXact_editor_populate( SchedXactionEditorDialog * );

static void new_button_clicked( GtkButton *b, gpointer d );
static void edit_button_clicked( GtkButton *b, gpointer d );
static void delete_button_clicked( GtkButton *b, gpointer d );
static void close_button_clicked( GtkButton *b, gpointer d );

static void endgroup_rb_toggled( GtkButton *b, gpointer d );
static void set_endgroup_toggle_states( SchedXactionEditorDialog *sxed, EndType t );
static void advance_toggle( GtkButton *b, SchedXactionEditorDialog *sxed );


/* ledger standard-handlers */
static gncUIWidget sxe_ledger_get_parent( GNCLedgerDisplay *ld );

/* ledger callbacks */
static void sxe_register_record_cb( GnucashRegister *reg, gpointer d );
static void sxe_register_redraw_all_cb( GnucashRegister *reg, gpointer d );

static void sxed_reg_recordCB( GtkWidget *w, gpointer d );
static void sxed_reg_cancelCB( GtkWidget *w, gpointer d );
static void sxed_reg_deleteCB( GtkWidget *w, gpointer d );
static void sxed_reg_duplicateCB( GtkWidget *w, gpointer d );
static void sxed_reg_expand_trans_checkCB( GtkWidget *w, gpointer d );
static void sxed_reg_new_transCB( GtkWidget *w, gpointer d );
static void sxed_reg_jumpCB( GtkWidget *w, gpointer d );
static void sxed_reg_xferCB( GtkWidget *w, gpointer d );

static void gnc_sxed_reg_check_close(SchedXactionEditorDialog *sxed);

gboolean _editor_component_sx_equality( gpointer find_data,
                                        gpointer user_data );

/** Implementations *****************************************************/

static
void
sxd_close_handler ( gpointer user_data )
{
        SchedXactionDialog        *sxd = user_data;

        gnome_dialog_close( GNOME_DIALOG( sxd->dialog ) );
}

static
void
sxed_close_handler ( gpointer user_data )
{
        SchedXactionEditorDialog        *sxed = user_data;

        gnc_sxed_reg_check_close( sxed );

        gnc_ledger_display_close( sxed->ledger );
        sxed->ledger = NULL;

        g_free (sxed->sxGUIDstr);
        sxed->sxGUIDstr = NULL;

        gnome_dialog_close( GNOME_DIALOG( sxed->dialog ) );
}

static
void
close_button_clicked( GtkButton *b, gpointer d )
{
        sxd_close_handler( d );
}

static
void
editor_close_button_clicked( GtkButton *b, SchedXactionEditorDialog *sxed )
{
        gnc_close_gui_component_by_data( DIALOG_SCHEDXACTION_EDITOR_CM_CLASS,
                                         sxed );
}


static
void
editor_help_button_clicked(GtkButton *b, SchedXactionEditorDialog *sxed)
{
        gnc_help_window *help = gnc_help_window_new();
	gnc_help_window_show_help(help, HH_SXEDITOR, NULL);
	return;
}

static
void
editor_ok_button_clicked( GtkButton *b, SchedXactionEditorDialog *sxed )
{
        GNCBook *book;
        GtkWidget *o, *o2, *o3;
        GList *sxList;
        FreqSpec *fs;
        gint row;
        time_t tmpDate;
        GDate *gdate;

        gdate = g_date_new();

        /* read out data back into SchedXaction object. */
        o = glade_xml_get_widget( sxed->gxml, "sxe_name" );
        xaccSchedXactionSetName( sxed->sx, gtk_entry_get_text( GTK_ENTRY(o) ) );

        o = glade_xml_get_widget( sxed->gxml, "rb_enddate" );
        o2 = glade_xml_get_widget( sxed->gxml, "rb_noend" );
        o3 = glade_xml_get_widget( sxed->gxml, "rb_num_occur" );
        if ( gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(o)) ) {
                /* get the end date data */
                o = glade_xml_get_widget( sxed->gxml, "sxe_end_date" );
                g_date_set_time( gdate, gnome_date_edit_get_date( GNOME_DATE_EDIT(o) ) );
                xaccSchedXactionSetEndDate( sxed->sx, gdate );
                /* set the num occurances data */
                xaccSchedXactionSetNumOccur( sxed->sx, -1 );
        } else if ( gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(o3) ) ) {
                /* get the occurances data */
                o = glade_xml_get_widget( sxed->gxml, "end_nentry" );
                xaccSchedXactionSetNumOccur( sxed->sx,
                                             (gint)gnome_number_entry_get_number( GNOME_NUMBER_ENTRY(o) ) );
                g_date_clear( gdate, 1 );
                xaccSchedXactionSetEndDate( sxed->sx, gdate );
        } else if ( gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON(o2) ) ) {
                xaccSchedXactionSetNumOccur( sxed->sx, -1 );
                g_date_clear( gdate, 1 );
                xaccSchedXactionSetEndDate( sxed->sx, gdate );
        } else {
                PERR( "No valid end specified\n" );
        }

        {
                gboolean autocreateState, notifyState;

                o = glade_xml_get_widget( sxed->gxml, "autocreate_opt" );
                autocreateState = gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON(o) );
                o = glade_xml_get_widget( sxed->gxml, "notify_opt" );
                notifyState = gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON(o) );

                /* "Notify" only makes sense if AutoCreate is actived;
                   enforce that here. */
                xaccSchedXactionSetAutoCreate( sxed->sx,
                                               autocreateState,
                                               (autocreateState & notifyState) );
        }
                                       
        {
                int daysInAdvance;

                daysInAdvance = 0;
                o = glade_xml_get_widget( sxed->gxml, "advance_opt" );
                if ( gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON(o) ) ) {
                        o = glade_xml_get_widget( sxed->gxml, "advance_days" );
                        daysInAdvance =
                                gtk_spin_button_get_value_as_int( GTK_SPIN_BUTTON(o) );
                }
                xaccSchedXactionSetAdvanceCreation( sxed->sx, daysInAdvance );

                daysInAdvance = 0;
                o = glade_xml_get_widget( sxed->gxml, "remind_opt" );
                if ( gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON(o) ) ) {
                        o = glade_xml_get_widget( sxed->gxml, "remind_days" );
                        daysInAdvance =
                                gtk_spin_button_get_value_as_int( GTK_SPIN_BUTTON(o) );
                }
                xaccSchedXactionSetAdvanceReminder( sxed->sx, daysInAdvance );
        }

        /* get the frequency spec data */
        fs = xaccSchedXactionGetFreqSpec( sxed->sx );
        gnc_frequency_save_state( sxed->gncfreq, fs, gdate );

        /* now that we have it, set the start date */
        xaccSchedXactionSetStartDate( sxed->sx, gdate );

        g_date_free( gdate );

        /* add to list */
        putSchedXactionInClist( sxed->sx, sxed->sxd );
        if ( sxed->new ) {
                book = gnc_get_current_book ();
                sxList = gnc_book_get_schedxactions( book );
                sxList = g_list_append( sxList, sxed->sx );
                gnc_book_set_schedxactions( book, sxList );
                sxed->sx = NULL;
        }

        /* cleanup */
        gnc_close_gui_component_by_data (DIALOG_SCHEDXACTION_EDITOR_CM_CLASS,
                                         sxed);
}

static void
autocreate_toggled( GtkObject *o, SchedXactionEditorDialog *sxed )
{
        GtkWidget *notifyCheck;

        notifyCheck = glade_xml_get_widget( sxed->gxml, "notify_opt" );
        gtk_widget_set_sensitive( notifyCheck,
                                  gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON(o) ) );
}

static void
advance_toggle( GtkButton *o, SchedXactionEditorDialog *sxed )
{
        gchar *spinName;
        GtkWidget *spin;

        spinName = (gchar*)gtk_object_get_data( GTK_OBJECT(o), "whichOneAmI" );
        spin = glade_xml_get_widget( sxed->gxml, spinName );
        if ( !spin ) {
                PERR( "Error getting widget with name \"%s\"", spinName );
                return;
        }
        gtk_widget_set_sensitive( spin,
                                  gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON(o) ) );
}

/* Local destruction of dialog */
static void
scheduledxaction_dialog_destroy(GtkObject *object, gpointer data)
{
        SchedXactionDialog *sxd = data;

        if (!sxd) return;

        gnc_unregister_gui_component_by_data
          (DIALOG_SCHEDXACTION_CM_CLASS, sxd);

        g_free(sxd);
}

/* Local destruction of dialog */
static void
scheduledxaction_editor_dialog_destroy(GtkObject *object, gpointer data)
{
        SchedXactionEditorDialog *sxed = data;

        if (sxed == NULL)
                return;

        gnc_unregister_gui_component_by_data
          (DIALOG_SCHEDXACTION_EDITOR_CM_CLASS, sxed);

        sxed->sx = NULL;

        g_free (sxed);
}

SchedXactionDialog*
gnc_ui_scheduled_xaction_dialog_create(void)
{
        SchedXactionDialog *sxd = NULL;
        GtkObject *sxdo;
        GtkWidget *button;
        GtkWidget *clist;
        GList *alreadyExisting = NULL;

        alreadyExisting = 
                gnc_find_gui_components( DIALOG_SCHEDXACTION_CM_CLASS,
                                         NULL,
                                         (gpointer)sxd );
        if ( alreadyExisting != NULL ) {
                sxd = (SchedXactionDialog*)alreadyExisting->data;
                gtk_widget_show( sxd->dialog );
                gdk_window_raise( sxd->dialog->window );
                g_list_free( alreadyExisting );
                return sxd;
        }

        sxd = g_new0( SchedXactionDialog, 1 );

        /* sxd->dialog = create_Scheduled_Transaction_List(); */
        sxd->gxml = gnc_glade_xml_new( "sched-xact.glade",
                                       "Scheduled Transaction List" );
        sxd->dialog = glade_xml_get_widget( sxd->gxml,
                                            "Scheduled Transaction List" );

        sxdo = GTK_OBJECT(sxd->dialog);

        gnc_register_gui_component( DIALOG_SCHEDXACTION_CM_CLASS,
                                    NULL, /* no refresh handler */
                                    sxd_close_handler, 
                                    sxd );

        gtk_signal_connect( sxdo, "destroy",
                            GTK_SIGNAL_FUNC(scheduledxaction_dialog_destroy),
                            sxd );

        button = glade_xml_get_widget( sxd->gxml, "new_button" );
        gtk_signal_connect( GTK_OBJECT(button), "clicked",
                            GTK_SIGNAL_FUNC(new_button_clicked), sxd );
        button = glade_xml_get_widget( sxd->gxml, "edit_button" );
        gtk_signal_connect( GTK_OBJECT(button), "clicked",
                            GTK_SIGNAL_FUNC(edit_button_clicked), sxd );
        button = glade_xml_get_widget( sxd->gxml, "delete_button" );
        gtk_signal_connect( GTK_OBJECT(button), "clicked",
                            GTK_SIGNAL_FUNC(delete_button_clicked), sxd );
        button = glade_xml_get_widget( sxd->gxml, "close_button" );
        gtk_signal_connect( GTK_OBJECT(button), "clicked",
                            GTK_SIGNAL_FUNC(close_button_clicked), sxd );

        schedXact_populate( sxd );

        gtk_widget_show(sxd->dialog);

        return sxd;
}

void
gnc_ui_scheduled_xaction_dialog_destroy(SchedXactionDialog *sxd)
{
        if (sxd == NULL)
                return;

        gnc_close_gui_component_by_data (DIALOG_SCHEDXACTION_CM_CLASS, sxd);
}

void
gnc_ui_scheduled_xaction_editor_dialog_destroy(SchedXactionEditorDialog *sxed)
{
        if (sxed == NULL)
                return;

        gnc_close_gui_component_by_data
          (DIALOG_SCHEDXACTION_EDITOR_CM_CLASS, sxed);
}

void
row_select_handler( GtkCList *clist,
                    gint row,
                    gint col,
                    GdkEventButton *event,
                    gpointer d )
{
        SchedXactionDialog                *sxd;
        SchedXactionEditorDialog        *sxed;
        SchedXaction                        *sx;
        
        sxd   = (SchedXactionDialog*)d;

        switch ( event->type ) {
        case GDK_2BUTTON_PRESS:
                sx = (SchedXaction*)gtk_clist_get_row_data( clist, row );
                sxed = gnc_ui_scheduled_xaction_editor_dialog_create( sxd, sx, 0 );
                break;
        default:
                /* noop */
                break;
        }
}

static
void
schedXact_populate( SchedXactionDialog *sxd )
{
        GNCBook *book;
        GList *sxList;
        GtkCList *sx_clist;
        FreqSpec *tmpFS;
        struct tm tmpTm;
        time_t tmpTime;
        int i;

        book = gnc_get_current_book ();
        sxList = gnc_book_get_schedxactions( book );

        g_list_foreach( sxList, putSchedXactionInClist, sxd );

        sx_clist = GTK_CLIST( glade_xml_get_widget( sxd->gxml,
                                                    "sched_xact_list" ) );
        for ( i=0; i<3; i++ ) {
                gtk_clist_set_column_auto_resize( sx_clist, i, TRUE );
        }
        gtk_signal_connect(GTK_OBJECT(sx_clist), "select-row",
                           GTK_SIGNAL_FUNC(row_select_handler), sxd );
}

SchedXactionEditorDialog *
gnc_ui_scheduled_xaction_editor_dialog_create( SchedXactionDialog *sxd,
                                               SchedXaction *sx,
                                               int newP )
{
        SchedXactionEditorDialog *sxed;
        GtkWidget *button;
        int i;
        GList *alreadyExists = NULL;

        static struct widgetSignalCallback {
                char     *name;
                char     *signal;
                void     (*fn)();
                gpointer objectData;
        } widgets[] = {
                { "ok_button",      "clicked", editor_ok_button_clicked,    NULL },
                { "cancel_button",  "clicked", editor_close_button_clicked, NULL },
		{ "help_button",    "clicked", editor_help_button_clicked,  NULL}, 

                { "rb_enddate",     "toggled", endgroup_rb_toggled,         GINT_TO_POINTER(END_OPTION) },
                { "rb_num_occur",   "toggled", endgroup_rb_toggled,         GINT_TO_POINTER(NUM_OCCUR_OPTION) },

                { "autocreate_opt", "toggled", autocreate_toggled,          NULL },
                { "advance_opt",    "toggled", advance_toggle,              (gpointer)"advance_days" },
                { "remind_opt",     "toggled", advance_toggle,              (gpointer)"remind_days" },

                { NULL,             NULL,      NULL,                        NULL }
        };

        alreadyExists = gnc_find_gui_components( DIALOG_SCHEDXACTION_EDITOR_CM_CLASS,
                                                 _editor_component_sx_equality,
                                                 sx );
        if ( alreadyExists ) {
                sxed = (SchedXactionEditorDialog*)alreadyExists->data;
                gtk_widget_show( sxed->dialog );
                gdk_window_raise( sxed->dialog );
                g_list_free( alreadyExists );
                return sxed;
        }


        sxed = g_new0( SchedXactionEditorDialog, 1 );

        /* sxed->dialog = create_Scheduled_Transaction_Editor(); */
        sxed->gxml = gnc_glade_xml_new( "sched-xact.glade", "Scheduled Transaction Editor" );
        sxed->dialog = glade_xml_get_widget( sxed->gxml, "Scheduled Transaction Editor" );
        if ( sxed->dialog == NULL ) {
                PERR( "Damnit... sxed->dialog is null\n" );
        }
        sxed->sxd = sxd;
        sxed->sx = sx;
        sxed->new = newP;

        gnc_register_gui_component( DIALOG_SCHEDXACTION_EDITOR_CM_CLASS,
                                    NULL, /* no refresh handler */
                                    sxed_close_handler, /* file-static
                                                           close handler */
                                    sxed );

        gtk_signal_connect(GTK_OBJECT(sxed->dialog), "destroy",
                           GTK_SIGNAL_FUNC(scheduledxaction_editor_dialog_destroy),
                           sxed);
        /* FIXME: want delete-event, too. */

        for ( i=0; widgets[i].name != NULL; i++ ) {
                button = glade_xml_get_widget( sxed->gxml, widgets[i].name );
                if ( widgets[i].objectData != NULL ) {
                        gtk_object_set_data( GTK_OBJECT(button),
                                             "whichOneAmI",
                                             widgets[i].objectData );
                }
                gtk_signal_connect( GTK_OBJECT(button),
                                    widgets[i].signal,
                                    GTK_SIGNAL_FUNC( widgets[i].fn ), sxed );
        }

        /* FIXME: For some reason the Glade-specified sensitivity settings
           are not being honored. */
        button = glade_xml_get_widget( sxed->gxml, "notify_opt" );
        gtk_widget_set_sensitive( button, FALSE );
        button = glade_xml_get_widget( sxed->gxml, "advance_days" );
        gtk_widget_set_sensitive( button, FALSE );
        button = glade_xml_get_widget( sxed->gxml, "remind_days" );
        gtk_widget_set_sensitive( button, FALSE );

        /* create the frequency-selection macrowidget */
        schedXact_editor_create_freq_sel( sxed );
        /* create the template-transaction ledger window */
        schedXact_editor_create_ledger( sxed );
        /* initialize */
        schedXact_editor_init( sxed );
        /* populate */
        schedXact_editor_populate( sxed );

        gtk_widget_show_all(sxed->dialog);

        return sxed;
}

static
void
schedXact_editor_init( SchedXactionEditorDialog *sxed )
{
        GtkWidget *w;
        w = glade_xml_get_widget( sxed->gxml,
                                  "end_nentry" );
        gtk_widget_set_sensitive( w, FALSE );
        /* Allow grow, allow shrink, auto-shrink */
        gtk_window_set_policy (GTK_WINDOW(sxed->dialog), TRUE, TRUE, FALSE);
}



static
GtkWidget *
schedXaction_editor_create_reg_popup( SchedXactionEditorDialog *sxed )
{
  GtkWidget *popup;

  static GnomeUIInfo transaction_menu[] =
  {
    {
      GNOME_APP_UI_ITEM,
      N_("_Enter"),
      N_("Record the current transaction"),
      sxed_reg_recordCB, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("_Cancel"),
      N_("Cancel the current transaction"),
      sxed_reg_cancelCB, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("_Delete"),
      N_("Delete the current transaction"),
      sxed_reg_deleteCB, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_ITEM,
      N_("D_uplicate"),
      N_("Make a copy of the current transaction"),
      sxed_reg_duplicateCB, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("_Schedule..."), 
      N_("Create a scheduled transaction using the current one as a template"),
      NULL, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL, 
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_TOGGLEITEM,
      N_("_Split"),
      N_("Show all splits in the current transaction"),
      sxed_reg_expand_trans_checkCB, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("_Blank"),
      N_("Move to the blank transaction at the "
         "bottom of the register"),
      sxed_reg_new_transCB, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("_Jump"),
      N_("Jump to the corresponding transaction in "
         "the other account"),
      sxed_reg_jumpCB, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_END
  };

  gnc_fill_menu_with_data( transaction_menu, sxed );

  popup = gnome_popup_menu_new (transaction_menu);

  return popup;
}

static GtkWidget *
schedXaction_editor_create_reg_tool_bar( SchedXactionEditorDialog *sxed )
{
        /* RegWindow *regData */
  GtkWidget *toolbar;

  static GnomeUIInfo toolbar_info[] =
  {
/*
    {
      GNOME_APP_UI_ITEM,
      N_("Close"),
      N_("Close this register window"),
      sxed_close_handler, NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_CLOSE,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
*/
    {
      GNOME_APP_UI_ITEM,
      N_("Enter"),
      N_("Record the current transaction"),
      sxed_reg_recordCB, NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_ADD,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("Cancel"),
      N_("Cancel the current transaction"),
      sxed_reg_cancelCB, NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_UNDELETE,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("Delete"),
      N_("Delete the current transaction"),
      sxed_reg_deleteCB, NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_TRASH,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_ITEM,
      N_("Duplicate"),
      N_("Make a copy of the current transaction"),
      sxed_reg_duplicateCB, NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_COPY,
      0, 0, NULL
    },
/*
    {
      GNOME_APP_UI_ITEM,
      N_("Schedule"),
      N_("Create a scheduled transaction using the current one as a template"),
      NULL, NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_LINE_IN,
      0, 0, NULL
    },
*/
    GNOMEUIINFO_SEPARATOR,
/*
    {
      GNOME_APP_UI_TOGGLEITEM,
      N_("Split"),
      N_("Show all splits in the current transaction"),
      expand_trans_cb, NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_BOOK_OPEN,
      0, 0, NULL
    },
*/
    {
      GNOME_APP_UI_ITEM,
      N_("Blank"),
      N_("Move to the blank transaction at the "
         "bottom of the register"),
      sxed_reg_new_transCB, NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_NEW,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("Jump"),
      N_("Jump to the corresponding transaction in "
         "the other account"),
      sxed_reg_jumpCB, NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_JUMP_TO,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_ITEM,
      N_("Transfer"),
      N_("Transfer funds from one account to another"),
      sxed_reg_xferCB, NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_CONVERT,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_ITEM,
      N_("Find"),
      N_("Find transactions with a search"),
      /* FIXME:gnc_ui_find_transactions_cb */ NULL,
      NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_SEARCH,
      0, 0, NULL
    },
/*
    {
      GNOME_APP_UI_ITEM,
      N_("Report"),
      N_("Open a report window for this register"),
      reportCB, 
      NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_BOOK_GREEN,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("Print"),
      N_("Print a report for this register"),
      printReportCB,
      NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_PRINT,
      0, 0, NULL
    },
*/
    GNOMEUIINFO_END
  };

  toolbar = gtk_toolbar_new (GTK_ORIENTATION_HORIZONTAL, GTK_TOOLBAR_BOTH);

  gnome_app_fill_toolbar_with_data (GTK_TOOLBAR(toolbar), toolbar_info,
                                    NULL, sxed);

  sxed->toolbar = toolbar;

  /* regData->split_button = toolbar_info[9].widget; */

  return toolbar;
}

static
void
schedXact_editor_create_freq_sel( SchedXactionEditorDialog *sxed )
{
        GtkFrame *f;

        f = GTK_FRAME( glade_xml_get_widget( sxed->gxml,
                                             "recur_frame" ) );
        
        sxed->gncfreq =
                GNC_FREQUENCY( gnc_frequency_new( xaccSchedXactionGetFreqSpec(sxed->sx),
                                                  xaccSchedXactionGetStartDate(sxed->sx) ) );
        if ( sxed->gncfreq == NULL ) {
                PERR( "gnc_frequency_new returned 0\n" );
                return;
        }
        gtk_container_add( GTK_CONTAINER(f), GTK_WIDGET(sxed->gncfreq) );
}

static
void
schedXact_editor_create_ledger( SchedXactionEditorDialog *sxed )
{
        GtkFrame *tempxaction_frame;
        SplitRegister *splitreg;
        GtkWidget *regWidget, *vbox, *popup, *toolbar;
#define NUM_LEDGER_LINES_DEFAULT 6
        int numLedgerLines = NUM_LEDGER_LINES_DEFAULT;

        tempxaction_frame =
                GTK_FRAME( glade_xml_get_widget( sxed->gxml,
                                                 "tempxaction_frame" ) );
        sxed->sxGUIDstr = guid_to_string( xaccSchedXactionGetGUID(sxed->sx) );
        sxed->ledger = gnc_ledger_display_template_gl( sxed->sxGUIDstr );

        gnc_ledger_display_set_handlers( sxed->ledger,
                                         NULL, sxe_ledger_get_parent );
        gnc_ledger_display_set_user_data( sxed->ledger, (gpointer)sxed );

        splitreg = gnc_ledger_display_get_split_register( sxed->ledger );

        numLedgerLines =
                (int)gnc_lookup_number_option( SX_OPT_STR,
                                               "Template Register Lines",
                                               NUM_LEDGER_LINES_DEFAULT );

        /* Watch the order of operations, here... */
        gnucash_register_set_initial_rows( numLedgerLines );
        regWidget = gnucash_register_new( splitreg->table );
        gnc_table_init_gui( regWidget, splitreg );
        sxed->reg = GNUCASH_REGISTER(regWidget);
        GNUCASH_SHEET(sxed->reg->sheet)->window = GTK_WIDGET(sxed->dialog);

        vbox = glade_xml_get_widget( sxed->gxml, "register_vbox" );

        popup = schedXaction_editor_create_reg_popup( sxed );
        gnucash_register_attach_popup( sxed->reg, popup, sxed );

        toolbar = schedXaction_editor_create_reg_tool_bar( sxed );
        gtk_container_set_border_width( GTK_CONTAINER(toolbar), 2 );

        gtk_container_add( GTK_CONTAINER(vbox), toolbar );
        gtk_container_add( GTK_CONTAINER(vbox), regWidget );

        /* FIXME: This doesn't actually do what we want. */
        gtk_box_set_child_packing( GTK_BOX(vbox), regWidget,
                                   TRUE, TRUE, 2,
                                   GTK_PACK_END /* it already has a
                                                 * position... */ );

#if 0
        gtk_signal_connect( GTK_OBJECT(sxed->dialog), "activate_cursor",
                            GTK_SIGNAL_FUNC(sxe_register_record_cb), sxed );
        gtk_signal_connect( GTK_OBJECT(sxed->dialog), "redraw_all",
                            GTK_SIGNAL_FUNC(sxe_register_redraw_all_cb), sxed );

#endif /* 0 */


        /* configure... */
        /* don't use double-line */
        gnc_split_register_config(splitreg,
                                  splitreg->type, splitreg->style,
                                  FALSE);

        /* don't show present/future divider [by definition, not necessary] */
        gnc_split_register_show_present_divider( splitreg, FALSE );

        /* force a refresh */
        gnc_ledger_display_refresh( sxed->ledger );
}

static
void
schedXact_editor_populate( SchedXactionEditorDialog *sxed )
{
        GtkEntry *nameEntry;
        char *name;
        time_t tmpDate;
        int numRec, numRecRem;
        GnomeDateEdit *gde;
        SplitRegister *splitReg;
        GList *splitList;
        GtkWidget *w;
        GString *tmpgStr;
        struct tm *tmpTm;
        GDate *gd;
        gint daysInAdvance;
        gboolean autoCreateState, notifyState;

        nameEntry = GTK_ENTRY(glade_xml_get_widget( sxed->gxml, "sxe_name" ));
        name = xaccSchedXactionGetName(sxed->sx);
        if ( name != NULL ) {
                gtk_entry_set_text( nameEntry, name  );
        }
        gd = xaccSchedXactionGetEndDate( sxed->sx );
        if ( g_date_valid( gd ) ) {
                w = glade_xml_get_widget( sxed->gxml, "rb_enddate" );
                gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON(w), TRUE );
                /* fill in date data. */
                w = glade_xml_get_widget( sxed->gxml, "sxe_end_date" );
                tmpTm = g_new0( struct tm, 1 );
                g_date_to_struct_tm( gd, tmpTm );
                tmpDate = mktime( tmpTm );
                g_free( tmpTm );
                gnome_date_edit_set_time( GNOME_DATE_EDIT(w), tmpDate );
                set_endgroup_toggle_states( sxed, END_DATE );
        } else if ( xaccSchedXactionGetNumOccur( sxed->sx ) != -1 ) {
                w = glade_xml_get_widget( sxed->gxml, "rb_num_occur" );
                gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON(w), TRUE );
                w = glade_xml_get_widget( sxed->gxml, "end_nentry" );
                w = gnome_number_entry_gtk_entry( GNOME_NUMBER_ENTRY(w) );
                tmpgStr = g_string_sized_new(5);
                g_string_sprintf( tmpgStr, "%d", xaccSchedXactionGetNumOccur( sxed->sx ) );
                gtk_entry_set_text( GTK_ENTRY(w), tmpgStr->str );
                g_string_free( tmpgStr, TRUE );
                set_endgroup_toggle_states( sxed, END_OCCUR );
        } else {
                w = glade_xml_get_widget( sxed->gxml, "rb_noend" );
                gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON(w), TRUE );
                set_endgroup_toggle_states( sxed, END_NONE );
        }

        /* Do auto-create/notify setup */
        if ( sxed->new ) {
                autoCreateState =
                        gnc_lookup_boolean_option( SX_OPT_STR,
                                                   "Auto-Create new Scheduled "
                                                   "Transactions by default", FALSE );
                notifyState =
                        gnc_lookup_boolean_option( SX_OPT_STR,
                                                   "Notify on new, auto-created "
                                                   "Scheduled Transactions", FALSE );
        } else {
                xaccSchedXactionGetAutoCreate( sxed->sx,
                                               &autoCreateState,
                                               &notifyState );
        }
        w = glade_xml_get_widget( sxed->gxml, "autocreate_opt" );
        gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON(w), autoCreateState );
        if ( ! autoCreateState ) {
                notifyState = FALSE;
        }
        w = glade_xml_get_widget( sxed->gxml, "notify_opt" );
        gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON(w), notifyState );


        /* Do days-in-advance-to-create widget[s] setup. */
        if ( sxed->new ) {
                daysInAdvance =
                        (int)gnc_lookup_number_option( SX_OPT_STR,
                                                       "Default number of days in "
                                                       "advance to create", 0 );
        } else {
                daysInAdvance =
                        xaccSchedXactionGetAdvanceCreation( sxed->sx );
        }
        if ( daysInAdvance != 0 ) {
                w = glade_xml_get_widget( sxed->gxml, "advance_opt" );
                gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON(w), TRUE );
                w = glade_xml_get_widget( sxed->gxml, "advance_days" );
                gtk_spin_button_set_value( GTK_SPIN_BUTTON(w),
                                           (gfloat)daysInAdvance );
        }

        /* Do days-in-advance-to-remind widget[s] setup. */
        if ( sxed->new ) {
                daysInAdvance =
                        (int)gnc_lookup_number_option( SX_OPT_STR,
                                                       "Default number of days in "
                                                       "advance to remind", 0 );
        } else {
                daysInAdvance =
                        xaccSchedXactionGetAdvanceReminder( sxed->sx );
        }
        if ( daysInAdvance != 0 ) {
                w = glade_xml_get_widget( sxed->gxml, "remind_opt" );
                gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON(w), TRUE );
                w= glade_xml_get_widget( sxed->gxml, "remind_days" );
                gtk_spin_button_set_value( GTK_SPIN_BUTTON(w),
                                           (gfloat)daysInAdvance );
        }

        /* populate the ledger */
        { 
                /* create the split list */
                GList        *splitList;

                splitList = xaccSchedXactionGetSplits( sxed->sx );
                if ( splitList != NULL ) {
                        splitReg = gnc_ledger_display_get_split_register
                          ( sxed->ledger );
                        gnc_split_register_load(splitReg, splitList, NULL );
                } /* otherwise, use the existing stuff. */
        }
}

static
void
set_endgroup_toggle_states( SchedXactionEditorDialog *sxed, EndType type )
{
        GtkWidget *dateCtl, *occurCtl;

        dateCtl = glade_xml_get_widget( sxed->gxml, "sxe_end_date" );
        gtk_widget_set_sensitive( GTK_WIDGET(dateCtl), (type == END_DATE) );

        occurCtl = glade_xml_get_widget( sxed->gxml, "end_nentry" );
        gtk_widget_set_sensitive( GTK_WIDGET(occurCtl), (type == END_OCCUR) );
}

static
void
new_button_clicked( GtkButton *b, gpointer d )
{
        SchedXactionDialog        *sxd;
        SchedXaction        *tmpSX =
                xaccSchedXactionMalloc( gnc_get_current_session ());
        SchedXactionEditorDialog *sxed;

        sxd = (SchedXactionDialog*)d;
        sxed = gnc_ui_scheduled_xaction_editor_dialog_create( sxd, tmpSX, 1 );
}

static
void
edit_button_clicked( GtkButton *b, gpointer d )
{
        GList                        *sel;
        GtkCList                *cl;
        int                        row;
        SchedXactionDialog        *sxd;
        SchedXaction                *sx;
        SchedXactionEditorDialog *sxed;

        sxd = (SchedXactionDialog*)d;

        cl = GTK_CLIST(glade_xml_get_widget( sxd->gxml, "sched_xact_list" ));
        sel = cl->selection;

        if ( !sel ) {
                return;
        }

        do {
                row = (int)sel->data;
                /* get the clist row for this listitem */
                /* get the object UD */
                sx = (SchedXaction*)gtk_clist_get_row_data( cl, row );
                sxed = gnc_ui_scheduled_xaction_editor_dialog_create( sxd, sx, 0 );
        } while ( (sel = g_list_next(sel)) );
}

static
void
delete_button_clicked( GtkButton *b, gpointer d )
{
        GNCBook *book;
        GtkCList *cl;
        GList *sel, *sxList;
        SchedXactionDialog *sxd;
        GnomeDialog *confirmDlg;
        GtkLabel *dlgMsgLbl;
        static char *confirmMessage =
                "Delete the selected scheduled transactions?";
        int confirmSel;
        GtkVBox *vb;
        GString *realMsg;
        SchedXaction *sx;

        sxd = (SchedXactionDialog*)d;

        cl = GTK_CLIST(glade_xml_get_widget( sxd->gxml, "sched_xact_list" ));
        sel = cl->selection;

        if ( !sel ) {
                return;
        }

        realMsg = g_string_new( confirmMessage );
        do {
                sx = gtk_clist_get_row_data( cl, (int)sel->data );
                g_string_sprintfa( realMsg, "\n\"%s\"", xaccSchedXactionGetName( sx ) );
        } while ( (sel = g_list_next(sel)) );

        confirmDlg =
                GNOME_DIALOG(gnome_dialog_new( "Confirm Delete",
                                               GNOME_STOCK_BUTTON_YES,
                                               GNOME_STOCK_BUTTON_NO,
                                               NULL ));
        dlgMsgLbl = GTK_LABEL(gtk_label_new( realMsg->str ));
        gtk_box_pack_start( GTK_BOX( confirmDlg->vbox ),
                            GTK_WIDGET(dlgMsgLbl), TRUE, TRUE, 0 );
        gnome_dialog_set_parent( confirmDlg,
                                 GTK_WINDOW(sxd->dialog) );
        gtk_widget_show_all( GTK_WIDGET(confirmDlg) );

        confirmSel = gnome_dialog_run_and_close( confirmDlg );
        g_string_free( realMsg, TRUE );

        switch ( confirmSel ) {
        case 0:
                sel = cl->selection;
                book = gnc_get_current_book ();
                sxList = gnc_book_get_schedxactions( book );
                do {
                        sx = (SchedXaction*)
                                gtk_clist_get_row_data( cl, (int)sel->data );
                        sxList = g_list_remove( sxList, (gpointer)sx );
                        xaccSchedXactionFree( sx );
                } while ( (sel = g_list_next(sel)) );
                gnc_book_set_schedxactions( book, sxList );

                gtk_clist_freeze( cl );
                gtk_clist_clear( cl );
                g_list_foreach( sxList, putSchedXactionInClist, sxd );
                gtk_clist_thaw( cl );
                break;
        case 1:
        default:
                return;
                break;
        }
}

static
void
endgroup_rb_toggled( GtkButton *b, gpointer d )
{
        /* figure out which one */
        SchedXactionEditorDialog        *sxed;
        gint id;

        sxed = (SchedXactionEditorDialog*)d;
        id = GPOINTER_TO_INT(gtk_object_get_data( GTK_OBJECT(b),
                                                  "whichOneAmI" ));

        /* FIXME: this can now be cleaned up with the help of
           set_endgroup_toggle_states(...) */
        switch (id) {
        case END_OPTION:
                set_endgroup_toggle_states( sxed, END_DATE );
                break;
        case NUM_OCCUR_OPTION:
                set_endgroup_toggle_states( sxed, END_OCCUR );
                break;
        default:
                g_error( "Unknown id %d", id );
                break;
        }
}

static
void
putSchedXactionInClist( gpointer data, gpointer user_data )
{
        SchedXaction *sx;
        SchedXactionDialog *sxd;
        GtkCList *clist;
        char *text[3];
        char *tmpStr;
        GString *freqStr;
        time_t nextTime;
        GString *nextDate;
        time_t now;
        gint row;
        int i;
        GDate gd;

        sx = (SchedXaction*)data;
        sxd = (SchedXactionDialog*)user_data;

        freqStr = g_string_new( "" );
        nextDate = g_string_new( "" );

        xaccFreqSpecGetFreqStr( xaccSchedXactionGetFreqSpec(sx), freqStr );

        gd = xaccSchedXactionGetNextInstance( sx );

        if ( ! g_date_valid( &gd ) ) {
                g_string_sprintf( nextDate, "not scheduled" );
        } else {
                char tmpBuf[26];
                       
                g_date_strftime( tmpBuf, 25, "%a, %b %e, %Y", &gd );
                g_string_sprintf( nextDate, "%s", tmpBuf );
        }

        text[0] = xaccSchedXactionGetName( sx );
        text[1] = freqStr->str;
        text[2] = nextDate->str;

        /* FIXME: leaky? */
        g_string_free( freqStr, FALSE );
        g_string_free( nextDate, FALSE );

        clist = GTK_CLIST( glade_xml_get_widget( sxd->gxml, "sched_xact_list" ) );
        gtk_clist_freeze( clist );
        row = gtk_clist_find_row_from_data( clist, sx );
        if ( row == -1 ) {
                row = gtk_clist_append( clist, text );
                gtk_clist_set_row_data( clist, row, sx );
        } else {
                for ( i=0; i<3; i++ ) {
                        gtk_clist_set_text( clist, row, i, text[i] );
                }
        }
        gtk_clist_thaw( clist );
}

static
gncUIWidget
sxe_ledger_get_parent( GNCLedgerDisplay *ld )
{
        SchedXactionEditorDialog *sxed;

        sxed = gnc_ledger_display_get_user_data( ld );
        /* FIXME: is this the direct parent?
           doesn't look like it from window-register.c:gnc_register_get_parent
        */
        return sxed->dialog;
}

static
void
sxe_register_record_cb( GnucashRegister *reg, gpointer d )
{
        DEBUG( "FIXME: sxe_register_record_cb called\n" );
}

static
void
sxe_register_redraw_all_cb( GnucashRegister *reg, gpointer d )
{
        DEBUG( "FIXME: sxe_register_redraw_all_cb called\n" );
}

static
void
sxed_reg_recordCB( GtkWidget *w, gpointer d )
{
        SchedXactionEditorDialog        *sxed = (SchedXactionEditorDialog*)d;
        SplitRegister        *reg;
        Transaction        *trans;

        reg = gnc_ledger_display_get_split_register( sxed->ledger );
        trans = gnc_split_register_get_current_trans( reg );
        if ( !gnc_split_register_save( reg, TRUE ) )
                return;

        gnc_split_register_redraw( reg );
}

static
void
sxed_reg_cancelCB( GtkWidget *w, gpointer d )
{
        gnc_split_register_cancel_cursor_trans_changes(
                gnc_ledger_display_get_split_register
                ( ((SchedXactionEditorDialog *)d)->ledger ) );
}

/* FIXME */
static void
refactor_transaction_delete_toggle_cb(GtkToggleButton *button, gpointer data)
{
  GtkWidget *text = gtk_object_get_user_data(GTK_OBJECT(button));
  gchar *s = data;
  gint pos = 0;

  gtk_editable_delete_text(GTK_EDITABLE(text), 0, -1);
  gtk_editable_insert_text(GTK_EDITABLE(text), s, strlen(s), &pos);
}

/* FIXME */
static DeleteType
refactor_transaction_delete_query(GtkWindow *parent)
{
  GtkWidget *dialog;
  GtkWidget *dvbox;
  GtkWidget *frame;
  GtkWidget *vbox;
  GtkWidget *trans_button;
  GtkWidget *splits_button;
  GtkWidget *text;
  GSList    *group;
  gint       pos = 0;
  gint       result;

  const char *usual = _("This selection will delete the whole "
                        "transaction. This is what you usually want.");
  const char *warn  = _("Warning: Just deleting all the splits will "
                        "make your account unbalanced. You probably "
                        "shouldn't do this unless you're going to "
                        "immediately add another split to bring the "
                        "transaction back into balance.");

  DeleteType return_value;

  dialog = gnome_dialog_new(_("Delete Transaction"),
                            GNOME_STOCK_BUTTON_OK,
                            GNOME_STOCK_BUTTON_CANCEL,
                            NULL);

  gnome_dialog_set_default(GNOME_DIALOG(dialog), 0);
  gnome_dialog_close_hides(GNOME_DIALOG(dialog), TRUE);
  gnome_dialog_set_parent(GNOME_DIALOG(dialog), parent);

  dvbox = GNOME_DIALOG(dialog)->vbox;

  frame = gtk_frame_new(NULL);
  gtk_container_border_width(GTK_CONTAINER(frame), 5);

  vbox = gtk_vbox_new(TRUE, 3);
  gtk_container_border_width(GTK_CONTAINER(vbox), 5);
  gtk_container_add(GTK_CONTAINER(frame), vbox);

  text = gtk_text_new(NULL, NULL);

  trans_button =
    gtk_radio_button_new_with_label(NULL,
                                    _("Delete the whole transaction"));
  gtk_object_set_user_data(GTK_OBJECT(trans_button), text);
  gtk_box_pack_start(GTK_BOX(vbox), trans_button, TRUE, TRUE, 0);

  gtk_signal_connect(GTK_OBJECT(trans_button), "toggled",
                     GTK_SIGNAL_FUNC(refactor_transaction_delete_toggle_cb),
                     (gpointer) usual);

  group = gtk_radio_button_group(GTK_RADIO_BUTTON(trans_button));
  splits_button = gtk_radio_button_new_with_label(group,
                                                  _("Delete all the splits"));
  gtk_object_set_user_data(GTK_OBJECT(splits_button), text);
  gtk_box_pack_start(GTK_BOX(vbox), splits_button, TRUE, TRUE, 0);

  gtk_signal_connect(GTK_OBJECT(splits_button), "toggled",
                     GTK_SIGNAL_FUNC(refactor_transaction_delete_toggle_cb),
                     (gpointer) warn);

  gtk_box_pack_start(GTK_BOX(dvbox), frame, TRUE, TRUE, 0);

  gtk_editable_insert_text(GTK_EDITABLE(text), usual, strlen(warn), &pos);
  gtk_text_set_line_wrap(GTK_TEXT(text), TRUE);
  gtk_text_set_word_wrap(GTK_TEXT(text), TRUE);
  gtk_text_set_editable(GTK_TEXT(text), FALSE);
  gtk_box_pack_start(GTK_BOX(dvbox), text, FALSE, FALSE, 0);

  gtk_widget_show_all(dvbox);

  result = gnome_dialog_run_and_close(GNOME_DIALOG(dialog));

  if (result != 0)
    return_value = DELETE_CANCEL;
  else if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(trans_button)))
    return_value = DELETE_TRANS;
  else if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(splits_button)))
    return_value = DELETE_SPLITS;
  else
    return_value = DELETE_CANCEL;

  gtk_widget_destroy(dialog);

  return return_value;
}

static
void
sxed_reg_deleteCB( GtkWidget *w, gpointer d )
{
  /* FIXME: this is C&P from window-register.c.  As it's the
     identical code [modulo the SplitReg userdata-lookup], it
     should be refactored. */
  SchedXactionEditorDialog *sxed = d;
  SplitRegisterStyle style;
  CursorClass cursor_class;
  SplitRegister *reg;
  Transaction *trans;
  char *buf = NULL;
  Split *split;
  gint result;

  reg = gnc_ledger_display_get_split_register (sxed->ledger);

  /* get the current split based on cursor position */
  split = gnc_split_register_get_current_split (reg);
  if (split == NULL)
  {
    gnc_split_register_cancel_cursor_split_changes (reg);
    return;
  }

  trans = xaccSplitGetParent(split);
  style = reg->style;
  cursor_class = gnc_split_register_get_current_cursor_class(reg);

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

  /* On a split cursor, just delete the one split. */
  if (cursor_class == CURSOR_CLASS_SPLIT)
  {
    const char *format = _("Are you sure you want to delete\n   %s\n"
                           "from the transaction\n   %s ?");
    /* ask for user confirmation before performing permanent damage */
    buf = g_strdup_printf(format, xaccSplitGetMemo(split),
                          xaccTransGetDescription(trans));

    /* result = gnc_verify_dialog_parented(sxed->dialog, buf, FALSE); */
    result = TRUE;

    g_free(buf);

    if (!result)
      return;

    gnc_split_register_delete_current_split (reg);
    return;
  }

  g_return_if_fail (cursor_class == CURSOR_CLASS_TRANS);

  /* On a transaction cursor with 2 or fewer splits in single or double
   * mode, we just delete the whole transaction, kerblooie */
  if ((xaccTransCountSplits(trans) <= 2) && (style == REG_STYLE_LEDGER))
  {
    const char *message = _("Are you sure you want to delete the current "
                            "transaction?");

    result = gnc_verify_dialog_parented(sxed->dialog, message, FALSE);

    if (!result)
      return;

    gnc_split_register_delete_current_trans (reg);
    return;
  }

  /* At this point we are on a transaction cursor with more than 2 splits
   * or we are on a transaction cursor in multi-line mode or an auto mode.
   * We give the user two choices: delete the whole transaction or delete
   * all the splits except the transaction split. */
  {
    DeleteType del_type;

    del_type = refactor_transaction_delete_query(GTK_WINDOW(sxed->dialog));

    if (del_type == DELETE_CANCEL)
      return;

    if (del_type == DELETE_TRANS)
    {
      gnc_split_register_delete_current_trans (reg);
      return;
    }

    if (del_type == DELETE_SPLITS)
    {
      gnc_split_register_emtpy_current_trans (reg);
      return;
    }
  }
}

static
void
sxed_reg_duplicateCB( GtkWidget *w, gpointer d )
{
        gnc_split_register_duplicate_current (
                gnc_ledger_display_get_split_register (
                        ((SchedXactionEditorDialog*)d)->ledger));
}

static
void
sxed_reg_expand_trans_checkCB( GtkWidget *w, gpointer d )
{
          SchedXactionEditorDialog *sxed = d;
          SplitRegister *reg;

          gnc_split_register_expand_current_trans
            (gnc_ledger_display_get_split_register (sxed->ledger),
             GTK_CHECK_MENU_ITEM (w)->active );
}

static
void
refactor_jump_to_blank( GNCLedgerDisplay *ledger,
                        GnucashRegister *gncReg )
{
          SplitRegister *reg = gnc_ledger_display_get_split_register (ledger);
          VirtualCellLocation vcell_loc;
          Split *blank;

          blank = gnc_split_register_get_blank_split (reg);
          if (blank == NULL)
                  return;

          if (gnc_split_register_get_split_virt_loc (reg, blank, &vcell_loc))
                  gnucash_register_goto_virt_cell (gncReg, vcell_loc);
}

static
void
sxed_reg_new_transCB( GtkWidget *w, gpointer d )
{
        SchedXactionEditorDialog *sxed = d;
        SplitRegister *reg;
        
        reg = gnc_ledger_display_get_split_register (sxed->ledger);
        
        if (gnc_split_register_save (reg, TRUE))
                gnc_split_register_redraw (reg);
        
        refactor_jump_to_blank( sxed->ledger, sxed->reg );
}

static
void
sxed_reg_jumpCB( GtkWidget *w, gpointer d )
{
        /* FIXME This one gets funky because of the accounts stored in
         * the kvp-frames */
}

static
void
sxed_reg_xferCB( GtkWidget *w, gpointer d )
{
        GnomeDialog *gd;
        /* FIXME: should use a "templatized" xfer dlg. */
}

/********************************************************************\
 * gnc_register_check_close                                         *
 *                                                                  *
 * Args:   regData - the data struct for this register              *
 * Return: none                                                     *
\********************************************************************/
static void
gnc_sxed_reg_check_close(SchedXactionEditorDialog *sxed)
{
        gboolean pending_changes;
        SplitRegister *reg;

        reg = gnc_ledger_display_get_split_register (sxed->ledger);

        pending_changes = gnc_split_register_changed (reg);
        if (pending_changes) {
                const char *message = _("The current transaction has been changed.\n"
                                        "Would you like to record it?");
                if (gnc_verify_dialog_parented(sxed->dialog, message, TRUE)) {
                        sxed_reg_recordCB(sxed->dialog, sxed);
                } else {
                        gnc_split_register_cancel_cursor_trans_changes (reg);
                }
        }
}

static gboolean
_editor_component_sx_equality( gpointer find_data,
                               gpointer user_data )
{
        return ( (SchedXaction*)find_data
                 == ((SchedXactionEditorDialog*)user_data)->sx );
}
