/********************************************************************\
 * dialog-sxsincelast.c - "since last run" dialog.                  *
 * Copyright (c) 2001 Joshua Sled <jsled@asynchronous.org>          *
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
#include <glib.h>

#include "gnc-ui.h"
#include "finvar.h"
#include "gnc-engine-util.h"
#include "FileDialog.h"
#include "gnc-book.h"
#include "Transaction.h"
#include "Group.h"
#include "gnc-numeric.h"
#include "SchedXaction.h"
#include "gnc-component-manager.h"
#include "SplitLedger.h"
#include "gnc-ui-util.h"
#include "gnc-exp-parser.h"
#include "dialog-utils.h"

#include "dialog-sxsincelast.h"

#define DIALOG_SXSINCELAST_CM_CLASS "dialog-sxsincelast"
#define DIALOG_SXSINCELAST_REMIND_CM_CLASS "dialog-sxsincelast-remind"

static short module = MOD_SX;

/**
 * There are two dialogs controled in this file: the since-last-run dialog,
 * and the reminder-dialog.  They cooperate and inter-relate, so they're both
 * handled here.
 **/
typedef struct _sxSinceLastData {
        GtkWidget *sxsincelastDlg;
        GladeXML *gxml;

        GtkWidget *dlg_remind;
        GladeXML *gxml_remind;

        gboolean sincelast_displayed;
        gboolean remind_displayed;

        GList /* <toCreateTuple*> */ *toCreateData;
        GList /* <reminderTuple*> */ *reminderData;
} sxSinceLastData;

typedef struct _toCreateTuple {
        SchedXaction *sx;
        GDate *date;
        gint clistRow;
        GHashTable *varBindings;
} toCreateTuple;

typedef struct _reminderTuple {
        SchedXaction *sx;
        GDate	*endDate;
        GDate	*occurDate;
        gboolean isSelected;
} reminderTuple;

/* Next reminder clist row index to create. */
static int rl_row = 0;
/* Next to-create clist row index to create. */
static int tcl_row = 0;
/* Next auto-create clist row index to create. */
static int acl_row = 0;

static void sxsincelast_init( sxSinceLastData *sxsld );

static gboolean sxsincelast_populate( sxSinceLastData *sxsld );
static void sxsincelast_close_handler( gpointer ud );
static void sxsld_remind_close_handler( gpointer ud );
static void sxsincelast_ok_clicked( GtkButton *b, gpointer ud );
static void sxsld_remind_ok_clicked( GtkButton *b, gpointer ud );
static void sxsincelast_cancel_clicked( GtkButton *b, gpointer ud );
static void sxsincelast_entry_changed( GtkEditable *e, gpointer ud );

static void sxsincelast_destroy( GtkObject *o, gpointer ud );

static void _create_transactions_on( SchedXaction *sx,
                                     GDate *gd,
                                     toCreateTuple *tct );
static gboolean _create_each_transaction_helper( Transaction *t, void *d );
static void _sxsl_get_sx_vars( SchedXaction *sx, GHashTable *varHash );
static void _hash_to_sorted_list( GHashTable *hashTable, GList **gl );
static void _andequal_numerics_set( gpointer key,
                                    gpointer value,
                                    gpointer data );
static void _print_vars_helper( gpointer key,
                                gpointer value,
                                gpointer user_data );
static void _clean_variable_table( sxSinceLastData *sxsld );

static void processAutoCreateList( GList *,
                                   sxSinceLastData *sxsld,
                                   SchedXaction * );
static void processToCreateList( GList *,
                                 sxSinceLastData *sxsld,
                                 SchedXaction * );
static void processReminderList( GList *,
                                 sxSinceLastData *sxsld,
                                 SchedXaction * );
static void processSelectedReminderList( GList *, sxSinceLastData * );

static void sxsincelast_tc_row_sel( GtkCList *clist,
                                    gint row, gint column,
                                    GdkEventButton *event,
                                    gpointer user_data);

static void sxsincelast_tc_row_unsel( GtkCList *clist,
                                      gint row, gint column,
                                      GdkEventButton *event,
                                      gpointer user_data );

static void sxsld_remind_row_toggle( GtkCList *clist,
                                     gint row, gint column,
                                     GdkEventButton *event,
                                     gpointer user_data);

static void _create_bad_reminders_msg( gpointer data, gpointer ud );
static gboolean _inform_or_add( GList *reminders,
                                reminderTuple *rt, gboolean okFlag,
                                GList *badList, GList **goodList );

int parse_vars_from_formula( const char *formula, GHashTable *varHash );

void
gnc_ui_sxsincelast_guile_wrapper( char *foo )
{
        gnc_ui_sxsincelast_dialog_create();
}

void
gnc_ui_sxsincelast_dialog_create(void)
{
        sxSinceLastData        *sxsld = g_new0( sxSinceLastData, 1 );

        sxsld->toCreateData = sxsld->reminderData = NULL;
        sxsld->remind_displayed = sxsld->sincelast_displayed = FALSE;

        sxsld->gxml = gnc_glade_xml_new( "sched-xact.glade",
                                         "Real Since-Last-Run Dialog" );
        sxsld->sxsincelastDlg =
                glade_xml_get_widget( sxsld->gxml,
                                      "Real Since-Last-Run Dialog" );


        sxsld->gxml_remind = gnc_glade_xml_new( "sched-xact.glade",
                                                "Since-Last Reminders" );
        sxsld->dlg_remind  = glade_xml_get_widget( sxsld->gxml_remind,
                                                   "Since-Last Reminders" );

        sxsincelast_init( sxsld );
}

static void
sxsincelast_init( sxSinceLastData *sxsld )
{
        GtkWidget *w;
        int i;
        struct widgetSignalHandlerTuple {
                char *name;
                char *signal;
                void (*handlerFn)();
        } widgets[] = {
                { "ok_button",       "clicked",      sxsincelast_ok_clicked },
                { "cancel_button",   "clicked",      sxsincelast_cancel_clicked },
                { "to_create_clist", "select-row",   sxsincelast_tc_row_sel },
                { "to_create_clist", "unselect-row", sxsincelast_tc_row_unsel },
                { NULL,              NULL,           NULL }
        }, widgets_remind[] = {
                { "ok_button",      "clicked",      sxsld_remind_ok_clicked },
                { "reminder_clist", "select-row",   sxsld_remind_row_toggle },
                { "reminder_clist", "unselect-row", sxsld_remind_row_toggle },
                { NULL,             NULL,           NULL }
        };

        gnc_register_gui_component( DIALOG_SXSINCELAST_CM_CLASS,
                                    NULL,
                                    sxsincelast_close_handler,
                                    sxsld->sxsincelastDlg );

        gnc_register_gui_component( DIALOG_SXSINCELAST_REMIND_CM_CLASS,
                                    NULL,
                                    sxsld_remind_close_handler,
                                    sxsld->dlg_remind );
        
        gtk_signal_connect( GTK_OBJECT(sxsld->sxsincelastDlg), "destroy",
                            GTK_SIGNAL_FUNC( sxsincelast_destroy ), sxsld );
        /* Note: we don't add a 'destroy' signal handler because the
           appropriate work [freeing since-last-run-related data structures]
           is done in the sxsincelast_destroy function. */

        for ( i=0; widgets[i].name != NULL ; i++ ) {
                w = glade_xml_get_widget( sxsld->gxml, widgets[i].name );
                gtk_signal_connect( GTK_OBJECT(w), widgets[i].signal,
                                    GTK_SIGNAL_FUNC(widgets[i].handlerFn),
                                    sxsld );
                
        }

        for ( i=0; widgets_remind[i].name != NULL ; i++ ) {
                w = glade_xml_get_widget( sxsld->gxml_remind,
                                          widgets_remind[i].name );
                gtk_signal_connect( GTK_OBJECT(w), widgets_remind[i].signal,
                                    GTK_SIGNAL_FUNC( widgets_remind[i].handlerFn ),
                                    sxsld );
        }

        /* set all to-create clist columns to auto-resize. */
        w = glade_xml_get_widget( sxsld->gxml, "to_create_clist" );
        for ( i=0; i<3; i++ ) {
                gtk_clist_set_column_auto_resize( GTK_CLIST(w), i, TRUE );
        }
        w = glade_xml_get_widget( sxsld->gxml_remind, "reminder_clist" );
        for ( i=0; i<3; i++ ) {
                gtk_clist_set_column_auto_resize( GTK_CLIST(w), i, TRUE );
        }

        rl_row = tcl_row = acl_row = 0;

        /* FIXME: deal with neither dialog being displayed [read:
           nothing-to-do.] */
        if ( sxsincelast_populate( sxsld ) ) {
                gtk_widget_show_all( sxsld->sxsincelastDlg );
                sxsld->sincelast_displayed = TRUE;
        }
}

static void
_generate_instances( SchedXaction *sx,
                     GDate *end,
                     GList **instanceList )
{
        GDate gd, *gdToReturn;
        char tmpBuf[26];

        gd = xaccSchedXactionGetNextInstance( sx );
        while ( g_date_valid(&gd)
                && g_date_compare( &gd, end ) <= 0 ) {

                g_date_strftime( tmpBuf, 25, "%a, %b %e, %Y", &gd );
                DEBUG( "Adding instance %s", tmpBuf );

                gdToReturn = g_date_new();
                *gdToReturn = gd;
                *instanceList = g_list_append( *instanceList, gdToReturn );

                gd = xaccSchedXactionGetInstanceAfter( sx, &gd );
        }
        if ( ! g_date_valid( &gd ) ) {
                PERR( "Should be added to dead-list..." );
        }
}

static void
_free_gdate_list_elts( gpointer data, gpointer user_data )
{
        g_date_free( (GDate*)data );
}

static void
_free_reminderTuple_list_elts( gpointer data, gpointer user_data )
{
        /* FIXME: endDate? */
        g_date_free( ((reminderTuple*)data)->occurDate );
        g_free( (reminderTuple*)data );
}

static void
_free_varBindings_hash_elts( gpointer key, gpointer value, gpointer data )
{
        g_free( key );
        g_free( value );
}

static void
_free_toCreate_list_elts( gpointer data, gpointer user_data )
{
        toCreateTuple *tct = (toCreateTuple*)data;
        tct->sx = NULL;
        g_date_free( tct->date );
        g_hash_table_foreach( tct->varBindings, _free_varBindings_hash_elts, NULL );
        g_hash_table_destroy( tct->varBindings );
        g_free( tct );
}

static void
processAutoCreateList( GList *autoCreateList, sxSinceLastData *sxsld, SchedXaction *sx )
{
        GtkCList *cl;
        gboolean autoCreateState, notifyState;
        char *rowText[2];

        /* get the "automagically created and notification requested"
         * register, and create the entries in it. For now, this is a clist,
         * but it _should_ be a GL...  */
        cl = GTK_CLIST( glade_xml_get_widget( sxsld->gxml, "auto_create_clist" ) );
        gtk_clist_freeze( cl );
        while ( autoCreateList ) {
                xaccSchedXactionGetAutoCreate( sx, &autoCreateState, &notifyState );
                _create_transactions_on( sx, (GDate*)autoCreateList->data, NULL );
                if ( notifyState ) {
                        rowText[0] = xaccSchedXactionGetName( sx );
                        rowText[1] = g_new0( gchar, 26 );
                        g_date_strftime( rowText[1], 25, "%a, %b %e, %Y",
                                         (GDate*)autoCreateList->data );
                        gtk_clist_insert( cl, acl_row++, rowText );
                        g_free( rowText[1] );
                }

                autoCreateList = autoCreateList->next;
        }
        gtk_clist_thaw( cl );

}

static void
processToCreateList( GList *toCreateList, sxSinceLastData *sxsld, SchedXaction *sx )
{
        toCreateTuple *tct;
        GtkCList *clist;
        char *rowText[3];

        if ( toCreateList == NULL )
                return;

        clist = GTK_CLIST( glade_xml_get_widget( sxsld->gxml, "to_create_clist" ) );
        do {
                tct = g_new0( toCreateTuple, 1 );
                tct->sx = sx;
                tct->date = (GDate*)toCreateList->data;
                tct->clistRow = tcl_row;
                tct->varBindings = g_hash_table_new( g_str_hash, g_str_equal );

                /* add to clist [ahem... register... ahem] */
                rowText[0] = xaccSchedXactionGetName( sx );
                rowText[1] = g_new0( char, 25 );
                g_date_strftime( rowText[1], 25, "%a, %b %e, %Y", tct->date );
                _sxsl_get_sx_vars( sx, tct->varBindings );

                if ( g_hash_table_size( tct->varBindings ) == 0 ) {
                        rowText[2] = "y";
                } else {
                        rowText[2] = "n";
                }
                gtk_clist_insert( clist, tcl_row, rowText );
                gtk_clist_set_row_data( clist, tcl_row, tct );
                tcl_row++;

                sxsld->toCreateData = g_list_append( sxsld->toCreateData, tct );

                g_free( rowText[1] );
                
        } while ( (toCreateList = toCreateList->next) );

        if ( tcl_row > 0 ) {
                gtk_clist_select_row( clist, 0, 0 );
        }
}

static void
processReminderList( GList *reminderList, sxSinceLastData *sxsld, SchedXaction *sx )
{
        GtkCList *clist;
        char *rowText[3];
        reminderTuple *rt;
        gboolean showIt;

        showIt = (g_list_length( reminderList ) > 0);

        if ( reminderList == NULL )
                return;

        clist = GTK_CLIST( glade_xml_get_widget( sxsld->gxml_remind, "reminder_clist" ) );

        do {
                rt = (reminderTuple*)reminderList->data;
                /* add to clist [ahem... register... ahem] */
                rowText[0] = xaccSchedXactionGetName( sx );
                rowText[1] = g_new0( gchar, 25 ); /* FIXME: appropriate size */
                g_date_strftime( rowText[1], 25, "%a, %b %e, %Y", rt->occurDate );
                rowText[2] = g_new0( gchar, 5 ); /* FIXME: appropriate size */
                sprintf( rowText[2], "%d",
                         (g_date_julian(rt->occurDate) - g_date_julian(rt->endDate)) );

                gtk_clist_insert( clist, rl_row, rowText );
                gtk_clist_set_row_data( clist, rl_row, (gpointer)rt );
                rl_row += 1;
                g_free( rowText[1] );
                g_free( rowText[2] );

                sxsld->reminderData = g_list_append( sxsld->reminderData, rt );

        } while ( (reminderList = reminderList->next) );

        if ( showIt ) {
                gtk_widget_show_all( sxsld->dlg_remind );
                sxsld->remind_displayed = TRUE;
        }
}

/**
 * Moves the selected reminders to the appropriate [auto-create or to-create]
 * sections of the since-last-run dialog.
 **/
static void
processSelectedReminderList( GList *goodList, sxSinceLastData *sxsld )
{
        reminderTuple *rt;
        gboolean autoCreateOpt, notifyOpt;
        GList *list;

        while ( goodList ) {
                rt = (reminderTuple*)goodList->data;

                list = NULL;

                xaccSchedXactionGetAutoCreate( rt->sx,
                                               &autoCreateOpt, &notifyOpt );
                if ( autoCreateOpt ) {
                        list = g_list_append( list,
                                              rt->occurDate );
                        processAutoCreateList( list, sxsld, rt->sx );
                } else {
                        list = g_list_append( list,
                                              rt->occurDate );
                        processToCreateList( list, sxsld, rt->sx );
                }
                /* FIXME: Cleanup appropriately? */
                g_list_free( list );
                list = NULL;

                goodList = goodList->next;
        }
}

/**
 * Returns TRUE if there's some populated in the dialog to show to the user,
 * FALSE if not.
 **/
static gboolean
sxsincelast_populate( sxSinceLastData *sxsld )
{

        GList *sxList, *instanceList;
        GList *autoCreateList, *toCreateList, *reminderList;
        SchedXaction *sx;
        GDate end, endPlusReminders;
        GDate *instDate;
        gint daysInAdvance;
        gboolean autocreateState, notifyState;
        GtkWidget *w;
        gint autoCreateRow, toCreateRow, remindersRow;
        reminderTuple *rt;
        gboolean showIt;

        showIt = FALSE;

        autoCreateList = toCreateList = reminderList = NULL;
        autoCreateRow  = toCreateRow  = remindersRow = 0;

        sxList = gnc_book_get_schedxactions( gncGetCurrentBook() );

        if ( sxList == NULL ) {
                DEBUG( "No scheduled transactions to populate." );
                return FALSE;
        }

        do {
                sx = (SchedXaction*)sxList->data;
                g_date_set_time( &end, time(NULL) );
                daysInAdvance = xaccSchedXactionGetAdvanceCreation( sx );
                g_date_add_days( &end, daysInAdvance );
                
                endPlusReminders = end;
                daysInAdvance = xaccSchedXactionGetAdvanceReminder( sx );
                g_date_add_days( &endPlusReminders, daysInAdvance );
                
  
#if 0
/* NOT USED */
                {
                        char tmpBuf[26];

                        g_date_strftime( tmpBuf, 25, "%a, %b %e, %Y", &end );
                        DEBUG( "We'll generate the appropriate instances now for "
                               "SX \"%s\" with end time %s...",
                               xaccSchedXactionGetName(sx),
                               tmpBuf );
                }
#endif /* 0 */
                instanceList = NULL;
                _generate_instances( sx, &endPlusReminders, &instanceList );

                if ( instanceList == NULL )
                        continue;

                xaccSchedXactionGetAutoCreate( sx, &autocreateState, &notifyState );
                do {
                        instDate = (GDate*)instanceList->data;
#if 0
/* NOT USED */
                        {
                                char tmpBuf[26];
                                g_date_strftime( tmpBuf, 25, "%a, %b %e, %Y", instDate );
                                DEBUG( "SX \"%s\" instance on %s",
                                       xaccSchedXactionGetName(sx), tmpBuf );
                        }
#endif /* 0 */

                        if ( (g_date_compare( &end, &endPlusReminders ) != 0)
                             && (g_date_compare( &end, instDate ) <= 0) ) {
                                rt = g_new0( reminderTuple, 1 );
                                rt->sx         = sx;
                                rt->endDate    = &end;
                                rt->occurDate  = instDate;
                                rt->isSelected = FALSE;
                                reminderList = g_list_append( reminderList, rt );
                        } else {
                                if ( autocreateState ) {
                                        autoCreateList = g_list_append( autoCreateList,
                                                                        instDate );
                                } else {
                                        toCreateList = g_list_append( toCreateList,
                                                                      instDate );
                                }
                        }
                } while ( (instanceList = instanceList->next) );

                /* Report RE:showing the dialog iff there's stuff in it to
                 * show. */
                showIt |= (g_list_length( autoCreateList ) > 0);
                showIt |= (g_list_length( toCreateList ) > 0);

                processAutoCreateList( autoCreateList, sxsld, sx );
                processReminderList  ( reminderList,   sxsld, sx );
                processToCreateList  ( toCreateList,   sxsld, sx );

                g_list_foreach( autoCreateList, _free_gdate_list_elts, NULL );
                g_list_free( autoCreateList );
                autoCreateList = NULL;

                /* We have moved the GDates over to the toCreateData list in
                   sxsld, so we don't free them here. */
                g_list_free( toCreateList );
                toCreateList = NULL;

                /* We have moved the reminderTuples over to the reminderData
                   list in sxsld, so we don't free them here. */
                g_list_free( reminderList );
                reminderList = NULL;

        } while ( (sxList = sxList->next) );

        return showIt;
}

static void
_clean_sincelast_dlg( sxSinceLastData *sxsld )
{
        GtkWidget *w;

        /* . clean out to-create clist
         * . free associated memories.
         */
        _clean_variable_table( sxsld );

        w = glade_xml_get_widget( sxsld->gxml, "auto_create_clist" );
        gtk_clist_clear( GTK_CLIST(w) );
        acl_row = 0;

        w = glade_xml_get_widget( sxsld->gxml, "to_create_clist" );
        gtk_clist_clear( GTK_CLIST(w) );
        g_list_foreach( sxsld->toCreateData, _free_toCreate_list_elts, sxsld );
        g_list_free( sxsld->toCreateData );
        sxsld->toCreateData = NULL;
        tcl_row = 0;

}

/**
 * Close policy:
 * . If reminders-dialog-open
 *   . clean up since-last and hide [reminders may need to re-populate and
 *     re-display it.
 * . If !reminders-dialog-open && since-last-dialog-open
 *   . close/destroy reminders dialog
 *   . close/destroy since-last dialog.
 * . If !reminders-dialog-open && !since-last-dialog-open
 *   . close/destroy reminders dialog.
 *   . close/destroy since-last dialog.
 **/
static void
sxsincelast_close_handler( gpointer ud )
{
        sxSinceLastData *sxsld = (sxSinceLastData*)ud;
        g_list_foreach( sxsld->toCreateData, _free_toCreate_list_elts, NULL );
        g_list_free( sxsld->toCreateData );
        sxsld->toCreateData = NULL;

        if ( sxsld->sincelast_displayed ) {
                DEBUG( "since-last dialog displayed" );
        }

        if ( sxsld->remind_displayed ) {
                DEBUG( "reminder dialog displayed, just hiding the since-last dialog." );
                /* FIXME: need to clean out old ui shit. */
                _clean_sincelast_dlg( sxsld );
                gtk_widget_hide( sxsld->sxsincelastDlg );
                sxsld->sincelast_displayed = FALSE;
        } else {
                DEBUG( "reminder dialog !displayed, closing both." );
                if ( sxsld->dlg_remind != NULL ) {
                        PERR( "How did the reminder dialog get !displayed, but !destroyed?" );
                        sxsld_remind_close_handler( sxsld );
                }

                gnome_dialog_close( GNOME_DIALOG( sxsld->sxsincelastDlg ) );
                sxsld->sxsincelastDlg = NULL;
                sxsld->sincelast_displayed = FALSE;
        }

        /* FIXME -- cleanup:
         * . reminder data?
         * . memory allocated for UI elts [labels and such]?
         * do this in the _destroy...
         */
}

static void
sxsld_remind_close_handler( gpointer ud )
{
        sxSinceLastData *sxsld;

        sxsld = (sxSinceLastData*)ud;

        gnome_dialog_close( GNOME_DIALOG( sxsld->dlg_remind ) );
        sxsld->dlg_remind = NULL;
        sxsld->remind_displayed = FALSE;
}

static void
free_elts( gpointer data, gpointer user_data )
{
        g_free( data );
}

static void
sxsincelast_ok_clicked( GtkButton *b, gpointer ud )
{
        sxSinceLastData *sxsld;
        GtkWidget *dlg;
        GtkWidget *o;
        GtkCList *cl;
        time_t gdeDate;
        GList *sxList;
        GNCBook *book;
        SchedXaction *sx;
        GDate gd, *endDate;
        gchar buf[1024];
        gint row;
        char *rowText[2];
        toCreateTuple *tct;

        GList *tcList;
        gboolean allVarsBound;

        sxsld = (sxSinceLastData*)ud;

        cl = GTK_CLIST(glade_xml_get_widget( sxsld->gxml, "to_create_clist" ));

        /* First: check to make sure all TCTs are 'ready' [and return if not].
         * Second: create the entries based on the variable bindings. */

        tcList = sxsld->toCreateData;
        if ( tcList == NULL ) {
                DEBUG( "No transactions to create..." );
                sxsincelast_close_handler( sxsld );
                return;
        }
        do {
                tct = (toCreateTuple*)tcList->data;
                allVarsBound = TRUE;
                g_hash_table_foreach( tct->varBindings,
                                      _andequal_numerics_set,
                                      &allVarsBound );
                if ( !allVarsBound ) {
                        char tmpBuf[26];
                        g_date_strftime( tmpBuf, 25, "%a, %b %e, %Y", tct->date );
                        /* FIXME: this should be better-presented to the user. */
                        DEBUG( "SX %s on date %s still has unbound variables.",
                               xaccSchedXactionGetName(tct->sx), tmpBuf );
                        gtk_clist_select_row( cl, tct->clistRow, 0 );
                        return;
                }
        } while ( (tcList = tcList->next) );

        tcList = sxsld->toCreateData;
        /* At this point we can assume there are to-create transactions and
           all variables are bound. */
        g_return_if_fail( tcList );
        do {
                tct = (toCreateTuple*)tcList->data;
                _create_transactions_on( tct->sx, tct->date, tct );
        } while ( (tcList = tcList->next) );

        /* FIXME: cleanup appropriately. */
        sxsincelast_close_handler( sxsld );

}

static void
sxsincelast_cancel_clicked( GtkButton *o, gpointer ud )
{
        sxsincelast_close_handler( ud );
}

static void
_andequal_numerics_set( gpointer key, gpointer value, gpointer data )
{
        gboolean *allVarsBound = data;
        *allVarsBound &= (value != NULL);
}

static void
sxsincelast_entry_changed( GtkEditable *e, gpointer ud )
{
        sxSinceLastData *sxsld;
        gchar *varName;
        toCreateTuple *tct;
        gchar *entryText;
        gnc_numeric *num;
        GHashTable *dummyVarHash;

        sxsld = (sxSinceLastData*)ud;
        tct = (toCreateTuple*)gtk_object_get_data( GTK_OBJECT(e), "tct" );
        varName = (gchar*)gtk_object_get_data( GTK_OBJECT(e), "varName" );
        entryText = gtk_editable_get_chars( e, 0, -1 );
        num = g_new0( gnc_numeric, 1 );
        *num = gnc_numeric_create( 0, 1 );
        dummyVarHash = g_hash_table_new( NULL, NULL );
        /* FIXME: these debugs probably want to go into a staus bar... */
        /* FIXME: Should be using xaccParseAmount instead of parser_parse_separate_vars? */
        if ( !gnc_exp_parser_parse_separate_vars( entryText, num, NULL, dummyVarHash ) ) {
                DEBUG( "error parsing entry \"%s\"", entryText  );
                g_free( num );
                num = NULL;
        } else if ( g_hash_table_size( dummyVarHash ) != 0 ) {
                DEBUG( "no new variables allowed in variable bindings for expression \"%s\"", entryText );
                g_free( num );
                num = NULL;
        } else if ( gnc_numeric_check( *num ) != GNC_ERROR_OK ) {
                DEBUG( "entry \"%s\" is not gnc_numeric-parseable", entryText );
                g_free( num );
                num = NULL;
        } else if ( gnc_numeric_zero_p( *num ) ) {
                DEBUG( "entry \"%s\" parses as '0'", ( entryText ? entryText : "(null)" ) );
                g_free( num );
                num = NULL;
        }

        g_hash_table_destroy( dummyVarHash );

        {
                gpointer maybeKey, maybeValue;

                if ( g_hash_table_lookup_extended( tct->varBindings, varName,
                                                   &maybeKey, &maybeValue ) ) {
                        g_hash_table_remove( tct->varBindings, maybeKey );
                        g_free( maybeValue );
                }
                g_hash_table_insert( tct->varBindings, maybeKey, num );
        }

        {
                GtkCList *clist;
                gboolean allVarsBound = TRUE;

                /* If there are no un-bound variables, then set the 'ready-to-go'
                   flag to 'y'. */
                g_hash_table_foreach( tct->varBindings, _andequal_numerics_set, &allVarsBound );
                clist = GTK_CLIST(glade_xml_get_widget( sxsld->gxml, "to_create_clist" ));
                gtk_clist_set_text( clist, tct->clistRow, 2, ( allVarsBound ? "y" : "n" ) );
        }
}

static void
sxsincelast_destroy( GtkObject *o, gpointer ud )
{
        /* appropriate place to destroy data structures */
        _clean_sincelast_dlg( (sxSinceLastData*)ud );
        /* FIXME: remains: reminder data. */
        DEBUG( "nuttin' doin...\n" );
}

static gboolean
_create_each_transaction_helper( Transaction *t, void *d )
{
        Transaction *newT;
        GList *sList;
        GList *osList;
        Split *split;
        kvp_frame *split_kvpf;
        kvp_value *kvp_val;
        gboolean errFlag;
        toCreateTuple *tct;
        gnc_commodity *commonCommodity = NULL;

        errFlag = FALSE;

        /* FIXME: In general, this should [correctly] deal with errors such
           as not finding the approrpiate Accounts and not being able to
           parse the formula|credit/debit strings. */

        /* FIXME: when we copy the trans_onto_trans, we don't want to copy
           the Split's kvp_frames... */

        DEBUG( "I'm seeing Transaction \"%s\"", xaccTransGetDescription( t ) );

        tct = (toCreateTuple*)d;

        newT = xaccMallocTransaction();
        xaccTransBeginEdit( newT );
        /* the action and description/memo are in the template */
        gnc_copy_trans_onto_trans( t, newT, FALSE, FALSE );

        /* the date is new [by definition :)] */
        xaccTransSetDate( newT,
                          g_date_day( tct->date ),
                          g_date_month( tct->date ),
                          g_date_year( tct->date ) );

        /* the accounts and amounts are in the kvp_frames of the splits. */
        osList = xaccTransGetSplitList( t );
        sList = xaccTransGetSplitList( newT );
        if ( (osList == NULL) || (sList == NULL) ) {
                PERR( "\tseen transaction w/o splits. :(" );
                return FALSE;
        }
        do {
                split = (Split*)sList->data;
                /* FIXME: Ick.  This assumes that the split lists will be
                   ordered identically. :( I think it's fair to say they
                   will, but I'd rather not have to count on it. --jsled */
                split_kvpf = xaccSplitGetSlots( (Split*)osList->data );

                DEBUG( "\tProcessing Split \"%s\"", xaccSplitGetMemo( split ) );
                DEBUG( "\tkvp_frame: %s\n", kvp_frame_to_string( split_kvpf ) );

                /* from-transaction of splits */
                {
                        GUID                *acct_guid;
                        Account                *acct;
                        /* contains the guid of the split's actual account. */
                        kvp_val = kvp_frame_get_slot( split_kvpf, "sched-xaction/xfrm" );
                        if ( kvp_val == NULL ) {
                                PERR( "Null kvp_val for xfrm" );
                        }
                        acct_guid = kvp_value_get_guid( kvp_val );
                        acct = xaccAccountLookup( acct_guid );
                        DEBUG( "Got account with name \"%s\"",
                                xaccAccountGetName( acct ) );
                        if ( commonCommodity != NULL ) {
                                if ( commonCommodity != xaccAccountGetCommodity( acct ) ) {
                                        PERR( "Common-commodity difference: old=%s, new=%s\n",
                                              gnc_commodity_get_mnemonic( commonCommodity ),
                                              gnc_commodity_get_mnemonic( xaccAccountGetCommodity( acct ) ) );
                                }
                        }
                        commonCommodity = xaccAccountGetCommodity( acct );
                        xaccAccountInsertSplit( acct, split );
                }
                /* credit/debit formulas */
                {
                        char *str, *parseErrorLoc;
                        gnc_numeric credit_num, debit_num, final;
                        int gncn_error;

                        kvp_val = kvp_frame_get_slot( split_kvpf, "sched-xaction/credit_formula" );
                        str = kvp_value_get_string( kvp_val );
                        credit_num = gnc_numeric_create( 0, 1 );
                        if ( str != NULL
                             && strlen(str) != 0 ) {
                                if ( ! gnc_exp_parser_parse_separate_vars( str, &credit_num,
                                                                           &parseErrorLoc, tct->varBindings ) ) {
                                        PERR( "Error parsing credit formula \"%s\" at \"%s\": %s",
                                              str, parseErrorLoc, gnc_exp_parser_error_string() );
                                        errFlag = TRUE;
                                        break;
                                }
                                
                                DEBUG( "gnc_numeric::credit: \"%s\" -> %s [%s]",
                                       str, gnc_numeric_to_string( credit_num ),
                                       gnc_numeric_to_string( gnc_numeric_reduce( credit_num ) ) );
                        }
                        
                        kvp_val = kvp_frame_get_slot( split_kvpf, "sched-xaction/debit_formula" );
                        str = kvp_value_get_string( kvp_val );

                        debit_num = gnc_numeric_create( 0, 1 );
                        if ( str != NULL
                             && strlen(str) != 0 ) {
                                if ( ! gnc_exp_parser_parse_separate_vars( str, &debit_num,
                                                                           &parseErrorLoc, tct->varBindings ) ) {
                                        PERR( "Error parsing debit_formula \"%s\" at \"%s\": %s",
                                              str, parseErrorLoc, gnc_exp_parser_error_string() );
                                        errFlag = TRUE;
                                        break;
                                }

                                DEBUG( "gnc_numeric::debit: \"%s\" -> %s [%s]",
                                       str, gnc_numeric_to_string( debit_num ),
                                       gnc_numeric_to_string( gnc_numeric_reduce( debit_num ) ) );
                        }
                        
                        final = gnc_numeric_sub_fixed( credit_num, debit_num );
                        
                        gncn_error = gnc_numeric_check( final );
                        if ( gncn_error != GNC_ERROR_OK ) {
                                PERR( "Error %d in final gnc_numeric value", gncn_error );
                                errFlag = TRUE;
                                break;
                        }
                        DEBUG( "gnc_numeric::final: \"%s\"",
                               gnc_numeric_to_string( final ) );
                        xaccSplitSetValue( split, final );
                }
#if 0
/* NOT [YET] USED */
                kvp_val = kvp_frame_get_slot( split_kvpf, "sched-xaction/shares" );
                kvp_val = kvp_frame_get_slot( split_kvpf, "sched-xaction/amnt" );
#endif /* 0 */

                /* FIXME:
                 *  . Want to save the scheduled transaction GUID in the
                 *    newly-created transactions/splits.
                 *  . Want to store the variable bindings, but this might be
                 *    problematic [if the formulas change in the SX].
                 */

        } while ( (sList = sList->next) && (osList = osList->next) );

        /* set the balancing currency. */
        if ( commonCommodity == NULL ) {
                PERR( "Unable to find common currency/commodity." );
        } else {
                xaccTransSetCurrency( newT, commonCommodity );
        }

        {
                kvp_frame *txn_frame;
                /* set a kvp-frame element in the transaction indicating and
                   pointing-to the SX this was created from. */
                txn_frame = xaccTransGetSlots( newT );
                if ( txn_frame == NULL ) {
                        txn_frame = kvp_frame_new();
                        xaccTransSetSlots_nc( newT, txn_frame );
                }
                kvp_val = kvp_value_new_guid( xaccSchedXactionGetGUID(tct->sx) );
                kvp_frame_set_slot( txn_frame, "from-sched-xaction", kvp_val );
        }

        if ( errFlag ) {
                PERR( "Some error in new transaction creation..." );
                xaccTransRollbackEdit( newT );
        } else {
                xaccTransCommitEdit( newT );
        }

        return TRUE;
}

/**
 * This should be called with the dates in increasing order, or the last call
 * will set the last occur date incorrectly.
 **/
static void
_create_transactions_on( SchedXaction *sx, GDate *gd, toCreateTuple *tct )
{
        AccountGroup *ag;
        Account *acct;
        char *id;
        char tmpBuf[26];

        {
                g_date_strftime( tmpBuf, 25, "%a, %b %e, %Y", gd );
                DEBUG( "Creating transactions on %s for %s",
                       tmpBuf, xaccSchedXactionGetName( sx ) );
        }

        if ( tct != NULL
             && g_date_compare( gd, tct->date ) != 0 ) {
                PERR( "GDate and TCT date aren't equal, which isn't a Good Thing." );
                return;
        }

        if ( tct == NULL ) {
                /* Create a faux tct for the creation-helper. */
                tct = g_new0( toCreateTuple, 1 );
                tct->sx = sx;
                tct->date = gd;
                tct->clistRow = -1;
        }

        ag = gnc_book_get_template_group( gncGetCurrentBook() );
        id = guid_to_string( xaccSchedXactionGetGUID(sx) );
        acct = xaccGetAccountFromName( ag, id );
        /* FIXME: handle not being able to get any of the above. */
        g_free( id );

        xaccAccountForEachTransaction( acct,
                                       _create_each_transaction_helper,
                                       tct );
        xaccSchedXactionSetLastOccurDate( sx, tct->date );
}

static void
_hashToList( gpointer key, gpointer value, gpointer user_data )
{
        *(GList**)user_data = g_list_append( *(GList**)user_data, key );
}

static void
_hash_to_sorted_list( GHashTable *hashTable, GList **gl )
{
        g_hash_table_foreach( hashTable, _hashToList, gl );
        *gl = g_list_sort( *gl, g_str_equal );
}

static void
_clear_variable_numerics( gpointer key, gpointer value, gpointer data )
{
        g_free( (gnc_numeric*)value );
        g_hash_table_insert( (GHashTable*)data, key, NULL );
}

static void
_sxsl_get_sx_vars( SchedXaction *sx, GHashTable *varHash )
{
        GList *splitList, *tmpL;
        kvp_frame *kvpf;
        kvp_value *kvp_val;
        Split *s;
        char *str;

        {
                AccountGroup *ag;
                Account *acct;
                char *id;

                ag = gnc_book_get_template_group( gncGetCurrentBook() );
                id = guid_to_string( xaccSchedXactionGetGUID(sx) );
                acct = xaccGetAccountFromName( ag, id );
                g_free( id );
                splitList = xaccAccountGetSplitList( acct );
        }

        if ( splitList == NULL ) {
                PINFO( "SchedXaction %s has no splits", xaccSchedXactionGetName( sx ) );
                return;
        }

        do {
                s = (Split*)splitList->data;

                kvpf = xaccSplitGetSlots(s);

                kvp_val = kvp_frame_get_slot( kvpf, "sched-xaction/credit_formula" );
                if ( kvp_val != NULL ) {
                        str = kvp_value_get_string( kvp_val );
                        if ( str && strlen(str) != 0 ) {
                                parse_vars_from_formula( str, varHash );
                        }
                }

                kvp_val = kvp_frame_get_slot( kvpf, "sched-xaction/debit_formula" );
                if ( kvp_val != NULL ) {
                        str = kvp_value_get_string( kvp_val );
                        if ( str && strlen(str) != 0 ) {
                                parse_vars_from_formula( str, varHash );
                        }
                }
        } while ( (splitList = splitList->next) );

        g_hash_table_foreach( varHash, _clear_variable_numerics, (gpointer)varHash );
}

static void
sxsincelast_tc_row_sel( GtkCList *clist,
                        gint row,
                        gint column,
                        GdkEventButton *event,
                        gpointer user_data)
{
        static const int NUM_COLS = 2;
        static GtkAttachOptions sopts = GTK_SHRINK;
        static GtkAttachOptions lxopts = GTK_EXPAND | GTK_FILL;
        GtkTable *varTable;
        int tableIdx;
        GtkWidget *label, *entry;
        GList *varList;

        toCreateTuple *tct;
        sxSinceLastData *sxsld;

        /* FIXME: this should more gracefully deal with multiple 'row-select'
           signals from double/triple-clicks. */
        sxsld = (sxSinceLastData*)user_data;

        tct = (toCreateTuple*)gtk_clist_get_row_data( clist, row );
        if ( tct == NULL ) {
                PERR( "TCT for row %d is NULL", row );
                return;
        }

        varList = NULL;
        _hash_to_sorted_list( tct->varBindings, &varList );

        if ( g_hash_table_size( tct->varBindings ) == 0 ) {
                PINFO( "No variables to deal with" );
                return;
        }

        varTable = GTK_TABLE( glade_xml_get_widget( sxsld->gxml, "variable_table" ) );
        gtk_table_resize( varTable, 4, NUM_COLS );

        tableIdx = 1;
        do {
                GString *gstr, *numValueStr;
                gnc_numeric *numValue;

                gstr = g_string_sized_new(16);
                g_string_sprintf( gstr, "%s: ", (gchar*)varList->data );
                label = gtk_label_new( gstr->str );
                gtk_label_set_justify( GTK_LABEL(label), GTK_JUSTIFY_RIGHT );
                g_string_free( gstr, TRUE );

                entry = gtk_entry_new();
                gtk_object_set_data( GTK_OBJECT(entry), "varName", varList->data );
                gtk_object_set_data( GTK_OBJECT(entry), "tct", tct );
                gtk_widget_set_usize( entry, 64, 0 );
                numValue = (gnc_numeric*)g_hash_table_lookup( tct->varBindings, varList->data );
                if ( numValue != NULL ) {
                        numValueStr = g_string_sized_new( 32 );
                        /* FIXME: Should use xaccPrintAmount(...) */
                        g_string_sprintf( numValueStr, "%.2f", gnc_numeric_to_double( *numValue ) );
                        gtk_entry_set_text( GTK_ENTRY(entry), numValueStr->str );
                        g_string_free( numValueStr, TRUE );
                }

                gtk_signal_connect( GTK_OBJECT(entry), "changed",
                                    GTK_SIGNAL_FUNC( sxsincelast_entry_changed ),
                                    sxsld );

                gtk_table_attach( varTable, label,
                                  0, 1, tableIdx, tableIdx + 1,
                                  lxopts, sopts, 0, 0 );
                gtk_table_attach( varTable, entry,
                                  1, 2, tableIdx, tableIdx + 1,
                                  sopts, sopts, 0, 0 );
                tableIdx += 1;
        } while ( (varList = varList->next) );

        gtk_widget_show_all( GTK_WIDGET(varTable) );
}

static void
_clean_variable_table( sxSinceLastData *sxsld )
{
        GtkTable *table;
        GList *children, *toFree;
        GtkTableChild *child;
        
        table = GTK_TABLE( glade_xml_get_widget( sxsld->gxml, "variable_table" ) );
        children = table->children;
        toFree = NULL;
        if ( children == NULL ) {
                PERR( "The variable-binding table should always have at least 2 children..." );
                return;
        }

        do {
                /* Destroy all children after the first [label-continaing]
                   row... ie, leave the labels in place. */
                child = (GtkTableChild*)children->data;
                if ( child->top_attach > 0 ) {
                        toFree = g_list_append( toFree, child->widget );
                }
        } while ( (children = children->next) );

        gtk_table_resize( table, 1, 2 );

        while ( toFree != NULL ) {
                gtk_widget_destroy( (GtkWidget*)toFree->data );
                toFree = toFree->next;
        }
        g_list_free( toFree );
}

static void
sxsincelast_tc_row_unsel( GtkCList *clist,
                          gint row, gint column,
                          GdkEventButton *event,
                          gpointer user_data )
{
        sxSinceLastData *sxsld;

        sxsld = (sxSinceLastData*)user_data;
        _clean_variable_table( sxsld );
}

static void
_print_vars_helper( gpointer key, gpointer value, gpointer user_data )
{
        DEBUG( "\"%s\" -> %.8x [%s]",
               (gchar*)key, (unsigned int)value,
               gnc_numeric_to_string( *(gnc_numeric*)value ) );
}

int
parse_vars_from_formula( const char *formula, GHashTable *varHash )
{
        gnc_numeric numeric;
        char *foo;
        
        if ( ! gnc_exp_parser_parse_separate_vars( formula, &numeric, &foo, varHash ) ) {
                PERR( "Error parsing at \"%s\": %s",
                        foo, gnc_exp_parser_error_string() );
                return -1;
        }

#if 0
/* NOT USED... still keep? */
        if ( g_hash_table_size( varHash ) == 0 ) {
                DEBUG( "No variables in expression \"%s\"", formula );
        } else {
                g_hash_table_foreach( varHash, _print_vars_helper, NULL );
        }
#endif /* 0 */

        return 0;
}

/**
 * The following makes me [jsled] somewhat sad, but it works... :I
 *
 * Basic problem: You can't create a SX instance if any after it have already
 * been created [e.g., if an SX has instances on d_0, d_1 and d_2, and you
 * create d_0 and d_2, then d_1 will never get created ... only d_3, d_4,
 * &c.]
 *
 * This code, then, makes sure that the user hasn't skipped a date...
 *
 * Code flow...
 * . If non-consecutive Reminders chosen, disallow.
 * . Else, for each selected reminder, add to to-create list.
 * . Dismiss dialog.
 **/
static void
sxsld_remind_ok_clicked( GtkButton *b, gpointer ud )
{
        sxSinceLastData *sxsld;
        GList *reminderList;
        reminderTuple *rt, *prevRT;
        char *rtName;
        gboolean overallOkFlag, okFlag, prevState;
        GList *badList;
        GList *badRecentRun;
        GList *goodList;

        rtName = NULL;
        goodList = NULL;
        overallOkFlag = TRUE;

        /* The following assignments are only to placate the compiler ["might
           be used uninitialized"]. */
        okFlag = prevState = TRUE;
        badList = badRecentRun = NULL;
        rt = prevRT = NULL;

        sxsld = (sxSinceLastData*)ud;
        reminderList = sxsld->reminderData;

        while ( reminderList != NULL ) {
                prevRT = rt;
                rt = (reminderTuple*)reminderList->data;

                if ( xaccSchedXactionGetName( rt->sx ) != rtName ) {
                        if ( rtName != NULL ) {
                                /* Deal with previous sequence. */
                                overallOkFlag &=
                                        _inform_or_add( sxsld->reminderData,
                                                        prevRT, okFlag,
                                                        badList, &goodList );
                        }

                        /* Reset loop state vars */
                        okFlag = prevState = TRUE;
                        rtName = xaccSchedXactionGetName( rt->sx );

                        /* Cleanup */
                        if ( badList != NULL ) {
                                g_list_free( badList );
                        }
                        if ( badRecentRun != NULL ) {
                                g_list_free( badRecentRun );
                        }
                        badList = badRecentRun = NULL;
                }

                /* If we haven't seen an inconsistency, then we're still ok.
                 * "inconsistency": we can go from [isSelected ==] TRUE to
                 * FALSE, but not back again [w/in a SX sequence, which we
                 * are [by the outer loop]].  */
                if ( prevState ) {
                        prevState = rt->isSelected;
                        if ( ! rt->isSelected ) {
                                badRecentRun =
                                        g_list_append( badRecentRun, rt );
                        }
                } else {
                        if ( rt->isSelected ) {
                                okFlag = FALSE;
                                if ( g_list_length( badRecentRun ) > 0 ) {
                                        badList =
                                                g_list_concat( badList,
                                                               badRecentRun );
                                        badRecentRun = NULL;
                                }
                        } else {
                                badRecentRun =
                                        g_list_append( badRecentRun, rt );
                        }

                }

                reminderList = reminderList->next;
        }

        /* Deal with final sequence. */
        if ( rtName != NULL ) {
                overallOkFlag &= _inform_or_add( sxsld->reminderData,
                                                 rt, okFlag,
                                                 badList, &goodList );
        }

        /* cleanup */
        g_list_free( badList );
        g_list_free( badRecentRun );

        /* Handle implications of above logic. */
        if ( overallOkFlag ) {
                /* Go through to-create list and do so...
                 * . if auto-create, add to auto-create list.
                 * . if to-create, add to to-create list.
                 * . if need to bring up since-last dialog, do so.
                 * . dismiss reminder dialog
                 * . if since-last dialog is empty, close.
                 */
                if ( g_list_length( goodList ) > 0 ) {
                        /* gtk_widget_hide( sxsld->sxsincelastDlg ); */
                        processSelectedReminderList( goodList, sxsld );
                        gtk_widget_show_all( sxsld->sxsincelastDlg );
                        sxsld->sincelast_displayed = TRUE;
                }
                g_list_free( goodList );

                sxsld_remind_close_handler( sxsld );

                /* FIXME: sufficient? we probably want the close-handler...
                gtk_widget_destroy( sxsld->dlg_remind );
                sxsld->dlg_remind = NULL; */
        }
        /* else
           { Just leave the dialog alone [and let the user deal with the
              error dialog they just saw].  }
        */
}

static void
sxsld_remind_row_toggle( GtkCList *clist,
                         gint row, gint column,
                         GdkEventButton *event,
                         gpointer user_data)
{
        reminderTuple *rt;
        rt = (reminderTuple*)gtk_clist_get_row_data( clist, row );
        rt->isSelected = !rt->isSelected;
}

static void
_create_bad_reminders_msg( gpointer data, gpointer ud )
{
        GString *msg;
        reminderTuple *rt;
        static char tmpBuf[26];

        rt = (reminderTuple*)data;
        msg = (GString*)ud;
        g_date_strftime( tmpBuf, 25, "%a, %b %e, %Y", rt->occurDate );
        g_string_sprintfa( msg, tmpBuf );
        g_string_sprintfa( msg, "\n" );
}

static gboolean
_inform_or_add( GList *reminders,
                reminderTuple *rt, gboolean okFlag,
                GList *badList, GList **goodList )
{
        reminderTuple *curRt;
        GList *rtPtr;
        GString *userMsg;

        userMsg = NULL;

        if ( okFlag ) {
                /* Add selected instances of this rt to
                   okay-to-add-to-toCreateList list. */
                rtPtr = reminders;
                if ( rtPtr == NULL ) {
                        PERR( "We should never see an empty reminder list." );
                }
                do {
                        curRt = (reminderTuple*)rtPtr->data;
                        /* this isn't really all that efficient. */
                        if ( (curRt->sx == rt->sx)
                             && curRt->isSelected ) {
                                *goodList = g_list_append( *goodList, curRt );
                        }
                } while( (rtPtr = rtPtr->next) );
        } else {
                /* [Add to list for later] dialog issuance to user. */
                userMsg = g_string_sized_new( 128 );
                g_string_sprintf( userMsg,
                                  "You cannot skip instances of Scheduled Transactions.\n"
                                  "The following instances of \"%s\"\n"
                                  "must be selected as well:\n\n",
                                  xaccSchedXactionGetName( rt->sx ) );
                g_list_foreach( badList, _create_bad_reminders_msg, userMsg );
                gnc_error_dialog( userMsg->str );
        }

        return okFlag;
}
