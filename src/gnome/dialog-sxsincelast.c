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

/**
 * . Page 1: reminders list
 *   . backed by: sxsld->reminderList
 * . Page 2: auto-create notify ledger
 *   . backed by: sxsld->autoCreateList [?]
 * . Page 3: to-create variable bindings
 *   . backed by: sxsld->toCreateData [s/Data/List/]
 * . Page 4: created ledger
 *   . backed by: sxsld->createdList [?]
 * . Page 5: obsolete list
 *   . backed by: sxsld->toRemoveList
 * . Page 6: finish
 *   . allow user last chance to revert/cancel changes.  If we need to do
 *   this, we'll go through our created lists and destroy a bunch of
 *   things. In the future, we'd rather use this page to implement changes
 *   after approval.
 **/

#include "config.h"

#include <gnome.h>
#include <glib.h>

#include "Group.h"
#include "Query.h"
#include "SchedXaction.h"
#include "Transaction.h"
#include "dialog-utils.h"
#include "finvar.h"
#include "gnc-book.h"
#include "gnc-book-p.h"
#include "gnc-component-manager.h"
#include "gnc-engine-util.h"
#include "gnc-exp-parser.h"
#include "gnc-numeric.h"
#include "gnc-ui-util.h"
#include "gnc-ui.h"
#include "gnc-gui-query.h"
#include "split-register.h"
#include "gnc-ledger-display.h"
#include "gnucash-sheet.h"
#include "gnc-regwidget.h"

#include "dialog-sxsincelast.h"

#ifdef HAVE_LANGINFO_D_FMT
#include <langinfo.h>
#endif

#define DIALOG_SXSINCELAST_CM_CLASS "dialog-sxsincelast"
#define DIALOG_SXSINCELAST_REMIND_CM_CLASS "dialog-sxsincelast-remind"
#define DIALOG_SXSINCELAST_OBSOLETE_CM_CLASS "dialog-sxsincelast-obsolete"

#define DIALOG_SXSINCELAST_GLADE_NAME "Since Last Run Druid"
#define SXSLD_DRUID_GLADE_NAME "sincelast_druid"

#define SINCELAST_DRUID   "sincelast_druid"
#define WHAT_TO_DO_PG "what_to_do"
#define REMINDERS_PG "reminders_page"
#define AUTO_CREATE_NOTIFY_PG "auto_create_notify_page"
#define TO_CREATE_PG "to_create_page"
#define CREATED_PG "created_page"
#define OBSOLETE_PG "obsolete_page"
#define FINISH_PG "finish_page"

#define SX_OBSOLETE_CLIST "sx_obsolete_clist"
#define TO_CREATE_CLIST "to_create_clist"
#define REMINDER_CLIST  "reminder_clist"
#define SX_GLADE_FILE "sched-xact.glade"

#define SELECT_ALL_BUTTON "select_all_button"
#define UNSELECT_ALL_BUTTON "unselect_all_button"
#define OK_BUTTON "ok_button"
#define CANCEL_BUTTON "cancel_button"
#define VARIABLE_TABLE "variables_table"
#define AUTO_CREATE_VBOX "ac_vbox"
#define CREATED_VBOX "created_vbox"
#define WHAT_TO_DO_VBOX "what_to_do_vbox"
#define WHAT_TO_DO_PROGRESS "creation_progress"

#define TO_CREATE_CLIST_WIDTH 3
#define REMINDER_CLIST_WIDTH  3
#define SX_OBSOLETE_CLIST_WIDTH 3

#define COERCE_VOID_TO_GBOOLEAN(x) ((gboolean)(*#x))

#ifdef HAVE_LANGINFO_D_FMT
#  define GNC_D_FMT (nl_langinfo (D_FMT))
#else
#  define GNC_D_FMT "%Y-%m-%d"
#endif

#define GNC_D_WIDTH 25
#define GNC_D_BUF_WIDTH 26

static short module = MOD_SX;

/**
 * The since-last-run dialog is a Gnome Druid which steps through the various
 * parts of scheduled transaction since-last-run processing; these parts are:
 *
 * 1. Display and select SX reminders for creation
 * 2. Show/allow editing of auto-created + notification-request SXes
 * 3. Show to-create SXes, allowing variable binding
 * 4. Show created SXes, allowing editing
 * 5. Allow deletion of any obsolete SXes
 *
 * Pages which aren't relevant are skipped; this is handled in the 'prep'
 * signal handler: e.g., a since-last dialog with only obsolete SXes would go
 * through the 'prep' methods of all it's pages to reach the Obsolete page.
 **/
typedef struct _sxSinceLastData {
        GtkWidget *sincelast_window;
        GnomeDruid *sincelast_druid;
        GladeXML *gxml;

        GtkProgressBar *prog;

        /* Multi-stage processing-related stuff... */
        GList /* <autoCreateTuple*> */ *autoCreateList;
        GList /* <toCreateTuple*> */   *toCreateList;
        GList /* <reminderTuple*> */   *reminderList;
        GList /* <toDeleteTuple*> */   *toRemoveList;

        /********** "Cancel"-related stuff... **********/
        
        /** A HashTable of SX mapped to initial temporal data. */
        GHashTable /* <SchedXaction*,void*> */ *sxInitStates;
        /** The list of removed SXes, in case we need to revert/cancel. */
        GList /* <toDeleteTuple*> */   *removedList;
        /** The list of GUIDs of Txns we've created... */
        GList /* <GUID*> */            *createdTxnGUIDList;

        gboolean autoCreatedSomething;
        gboolean createdSomething;

        GNCLedgerDisplay *ac_ledger;
        GNCRegWidget *ac_regWidget;

        GNCLedgerDisplay *created_ledger;
        GNCRegWidget *created_regWidget;

        /** Next reminder clist row index to create. **/
        gint rl_row;
        /** Next to-create clist row index to create. **/
        gint tcl_row;
} sxSinceLastData;

typedef struct autoCreateTuple_ {
        SchedXaction *sx;
        GDate *date;
} autoCreateTuple;

typedef struct toCreateTuple_ {
        SchedXaction *sx;
        GDate *date;
        gint clistRow;
        GHashTable *varBindings;
} toCreateTuple;

typedef struct reminderTuple_ {
        SchedXaction *sx;
        GDate	*endDate;
        GDate	*occurDate;
        gboolean isSelected;
} reminderTuple;

typedef struct toDeleteTuple_ {
        SchedXaction *sx;
        GDate *endDate;
        gchar *freq_info;
        gboolean isSelected;
} toDeleteTuple;

typedef struct creation_helper_userdata_ {
        /* the to-create tuple */
        toCreateTuple *tct;
        /* a [pointer to a] GList to append the GUIDs of newly-created
         * Transactions to, or NULL */
        GList **createdGUIDs;
} createData;

static void sxsincelast_init( sxSinceLastData *sxsld );
static void create_autoCreate_ledger( sxSinceLastData *sxsld );
static void create_created_ledger( sxSinceLastData *sxsld );
static gncUIWidget sxsld_ledger_get_parent( GNCLedgerDisplay *ld );

static gboolean sxsincelast_populate( sxSinceLastData *sxsld );
static void sxsincelast_druid_cancelled( GnomeDruid *druid, gpointer ud );
static void sxsincelast_close_handler( gpointer ud );

static void sxsincelast_entry_changed( GtkEditable *e, gpointer ud );
static void sxsincelast_destroy( GtkObject *o, gpointer ud );
static void create_transactions_on( SchedXaction *sx,
                                    GDate *gd,
                                    toCreateTuple *tct,
                                    GList **createdGUIDs );
static gboolean create_each_transaction_helper( Transaction *t, void *d );
void sxsl_get_sx_vars( SchedXaction *sx, GHashTable *varHash );
static void hash_to_sorted_list( GHashTable *hashTable, GList **gl );
static void andequal_numerics_set( gpointer key,
                                   gpointer value,
                                   gpointer data );
void print_vars_helper( gpointer key,
                        gpointer value,
                        gpointer user_data );
static void clean_sincelast_data( sxSinceLastData *sxsld );
static void clean_variable_table( sxSinceLastData *sxsld );

static void process_auto_create_list( GList *, sxSinceLastData *sxsld );
static void add_to_create_list_to_gui( GList *, sxSinceLastData *sxsld );
static void add_reminders_to_gui( GList *, sxSinceLastData *sxsld );
static void processRemoveList( GList *, sxSinceLastData *sxsld );
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

static gboolean processed_valid_reminders_listP( sxSinceLastData *sxsld );
static void create_bad_reminders_msg( gpointer data, gpointer ud );
static gboolean inform_or_add( GList *reminders,
                               reminderTuple *rt, gboolean okFlag,
                               GList *badList, GList **goodList );

int parse_vars_from_formula( const char *formula, GHashTable *varHash );

static void sx_obsolete_select_all_clicked(GtkButton *button,
                                           gpointer user_data);
static void sx_obsolete_unselect_all_clicked(GtkButton *button,
                                             gpointer user_data);

/**
 * Used to wrap for the book-open hook, where the book filename is given.
 **/
gboolean
gnc_ui_sxsincelast_guile_wrapper( char *bookfile )
{
        return gnc_ui_sxsincelast_dialog_create();
}

/**
 * Returns TRUE if the dialogs were created, FALSE if not.  The caller
 * probably wants to use this to inform the user in the manner appropriate to
 * the calling context.
 *
 * [i.e., for book-open-hook: do nothing; for menu-selection: display an info
 *  dialog stating there's nothing to do.]
 **/
gboolean
gnc_ui_sxsincelast_dialog_create()
{
        sxSinceLastData        *sxsld = g_new0( sxSinceLastData, 1 );

        sxsld->toCreateList = sxsld->reminderList = sxsld->toRemoveList = NULL;
        sxsld->sxInitStates = g_hash_table_new( g_direct_hash, g_direct_equal );

        if ( ! sxsincelast_populate( sxsld ) ) {
                g_free( sxsld );
                return FALSE;
        }

        sxsld->gxml = gnc_glade_xml_new( SX_GLADE_FILE,
                                         DIALOG_SXSINCELAST_GLADE_NAME );
        sxsld->sincelast_window =
                glade_xml_get_widget( sxsld->gxml,
                                      DIALOG_SXSINCELAST_GLADE_NAME );
        sxsld->sincelast_druid =
                GNOME_DRUID( glade_xml_get_widget( sxsld->gxml,
                                                   SXSLD_DRUID_GLADE_NAME ) );
        sxsincelast_init( sxsld );
        return TRUE;
}

static void 
clist_set_all_cols_autoresize( GtkCList *cl, guint n_cols )
{
        guint col;
        for( col = 0; col< n_cols; col++ ) {
                gtk_clist_set_column_auto_resize (cl, col, TRUE);
        }
        return;
}

typedef struct {
        char *name;
        char *signal;
        void (*handlerFn)();
} widgetSignalHandlerTuple;

typedef struct {
        char     *pageName;
        void     (*prepareHandlerFn)();
        gboolean (*backHandlerFn)();
        gboolean (*nextHandlerFn)();
        void     (*finishHandlerFn)();
        gboolean (*cancelHandlerFn)();
} druidSignalHandlerTuple;
   
static void
dialog_widgets_attach_handlers(GladeXML *dialog_xml, 
			       widgetSignalHandlerTuple *handler_info, 
			       sxSinceLastData *sxsld)
{
        int i;
        GtkWidget *w;

        for(i = 0; handler_info[i].name != NULL; i++)
        {
                w = glade_xml_get_widget(dialog_xml, handler_info[i].name);
                gtk_signal_connect( GTK_OBJECT(w), handler_info[i].signal, 
                                    GTK_SIGNAL_FUNC(handler_info[i].handlerFn),
                                    sxsld);
        }
}

static void
druid_pages_attach_handlers( GladeXML *dialog_xml,
                             druidSignalHandlerTuple *handler_info,
                             sxSinceLastData *sxsld )
{
        int i;
        GtkWidget *w;

        for(i = 0; handler_info[i].pageName != NULL; i++)
        {
                w = glade_xml_get_widget(dialog_xml, handler_info[i].pageName);
                if ( handler_info[i].prepareHandlerFn ) {
                        gtk_signal_connect( GTK_OBJECT(w), "prepare",
                                            GTK_SIGNAL_FUNC(handler_info[i].
                                                            prepareHandlerFn),
                                            sxsld);
                }
                if ( handler_info[i].backHandlerFn ) {
                        gtk_signal_connect( GTK_OBJECT(w), "back",
                                            GTK_SIGNAL_FUNC(handler_info[i].
                                                            backHandlerFn),
                                            sxsld);
                }
                if ( handler_info[i].nextHandlerFn ) {
                        gtk_signal_connect( GTK_OBJECT(w), "next",
                                            GTK_SIGNAL_FUNC(handler_info[i].
                                                            nextHandlerFn),
                                            sxsld);
                }
                if ( handler_info[i].finishHandlerFn ) {
                        gtk_signal_connect( GTK_OBJECT(w), "finish",
                                            GTK_SIGNAL_FUNC(handler_info[i].
                                                            finishHandlerFn),
                                            sxsld);
                }
                if ( handler_info[i].cancelHandlerFn ) {
                        gtk_signal_connect( GTK_OBJECT(w), "cancel",
                                            GTK_SIGNAL_FUNC(handler_info[i].
                                                            cancelHandlerFn),
                                            sxsld);
                }
        }
}

static void
sxsincelast_druid_cancelled( GnomeDruid *druid, gpointer ud )
{
        sxSinceLastData *sxsld = (sxSinceLastData*)ud;

        DEBUG( "druid cancelled" );
        gtk_widget_hide( sxsld->sincelast_window );
        sxsincelast_close_handler( sxsld );
}

static gboolean
theres_no_turning_back_bang( GnomeDruidPage *druid_page,
                             gpointer arg1, gpointer ud )
{
        DEBUG( "there's no turning back!!! MuHahahahhaha" );
        return TRUE;
}

static void
whattodo_prep( GnomeDruidPage *druid_page,
               gpointer arg1, gpointer ud )
{
        DEBUG( "whattodo_prep" );
}

static void
reminders_page_prep( sxSinceLastData *sxsld )
{
        GtkWidget *w;
        if ( g_list_length( sxsld->reminderList ) == 0 ) {
                w = glade_xml_get_widget( sxsld->gxml,
                                          AUTO_CREATE_NOTIFY_PG );
                DEBUG( "Going to auto_create_notify page" );
                gnome_druid_set_page( sxsld->sincelast_druid,
                                      GNOME_DRUID_PAGE(w) );
                return;
        }

        w = glade_xml_get_widget( sxsld->gxml, REMINDER_CLIST );
        gtk_clist_freeze( GTK_CLIST(w) );
        gtk_clist_clear( GTK_CLIST(w) );
        add_reminders_to_gui( sxsld->reminderList, sxsld );
        gtk_clist_thaw( GTK_CLIST(w) );
}

static void 
reminders_prep( GnomeDruidPage *druid_page,
                gpointer arg1, gpointer ud )
{
        sxSinceLastData *sxsld = (sxSinceLastData*)ud;
        reminders_page_prep( sxsld );
}

static gboolean 
reminders_next( GnomeDruidPage *druid_page,
                gpointer arg1, gpointer ud )
{
        sxSinceLastData *sxsld = (sxSinceLastData*)ud;
        if ( !processed_valid_reminders_listP( sxsld ) ) {
                return TRUE;
        }
        return FALSE;
}

#if 0
static gboolean 
reminders_back( GnomeDruidPage *druid_page,
                gpointer arg1, gpointer ud )
{
        DEBUG( "reminders_back" );
        return FALSE;
}
#endif

static void
reminders_finish( GnomeDruidPage *druid_page,
                  gpointer arg1, gpointer ud )
{
        DEBUG( "reminders_finish" );
}

static void
auto_create_prep( GnomeDruidPage *druid_page,
                  gpointer arg1, gpointer ud )
{
        GtkWidget *w;
        sxSinceLastData *sxsld = (sxSinceLastData*)ud;

        if ( ! sxsld->autoCreatedSomething ) {
                DEBUG( "Going to to_create page" );
                w = glade_xml_get_widget( sxsld->gxml, TO_CREATE_PG );
                gnome_druid_set_page( sxsld->sincelast_druid,
                                      GNOME_DRUID_PAGE(w) );
                return;
        }
}

static void
created_prep( GnomeDruidPage *druid_page,
              gpointer arg1, gpointer ud )
{
        GtkWidget *w;
        sxSinceLastData *sxsld = (sxSinceLastData*)ud;

        if ( !sxsld->createdSomething ) {
                DEBUG( "Going to obsolete page" );
                w = glade_xml_get_widget( sxsld->gxml, OBSOLETE_PG );
                gnome_druid_set_page( sxsld->sincelast_druid,
                                      GNOME_DRUID_PAGE(w) );
                return;
        }
}

static void
obsolete_prep( GnomeDruidPage *druid_page,
               gpointer arg1, gpointer ud )
{
        GtkWidget *w;
        sxSinceLastData *sxsld = (sxSinceLastData*)ud;
        if ( g_list_length( sxsld->toRemoveList ) == 0 ) {
                DEBUG( "Going to finish page" );
                w = glade_xml_get_widget( sxsld->gxml, FINISH_PG );
                gnome_druid_set_page( sxsld->sincelast_druid,
                                      GNOME_DRUID_PAGE(w) );
                return;
        }
        processRemoveList( sxsld->toRemoveList, sxsld );
}

static gboolean
obsolete_next( GnomeDruidPage *druid_page,
               gpointer arg1, gpointer ud )
{
        GList *sxList, *toDelPtr, *elt;
        GtkCList *cl;
        gint row;
        toDeleteTuple *tdt;
        sxSinceLastData *sxsld = (sxSinceLastData*)ud;

        DEBUG( "obsolete_next" );
        sxList = gnc_book_get_schedxactions( gnc_get_current_book() );
        cl = GTK_CLIST( glade_xml_get_widget( sxsld->gxml,
                                              SX_OBSOLETE_CLIST ) );

        for ( toDelPtr = cl->selection;
              toDelPtr;
              toDelPtr = toDelPtr->next ) {
                row = (gint)toDelPtr->data;
                DEBUG( "getting row data for SX_OBSOLETE_CLIST row %d",
                       row );
                tdt = (toDeleteTuple*)gtk_clist_get_row_data( cl, row );
                DEBUG( "got tdt with sx with name \"%s\"",
                       xaccSchedXactionGetName( tdt->sx ) );
                {
                        elt = g_list_find( sxList, tdt->sx );
                        sxList = g_list_remove_link( sxList, elt );
                        
                        sxsld->removedList = g_list_concat( sxsld->removedList, elt );
                }
                {
                        elt = g_list_find( sxsld->toRemoveList, tdt );
                        sxsld->toRemoveList =
                                g_list_remove_link( sxsld->toRemoveList, elt );
                        g_list_free_1(elt);
                }
                g_date_free( tdt->endDate );
                g_free( tdt );
        }

        gnc_book_set_schedxactions( gnc_get_current_book(), sxList );

        gtk_clist_freeze( cl );
        gtk_clist_clear( cl );
        gtk_clist_thaw( cl );

        return FALSE;
}

static void
to_create_prep( GnomeDruidPage *druid_page,
                gpointer arg1, gpointer ud )
{
        GtkWidget *w;
        sxSinceLastData *sxsld = (sxSinceLastData*)ud;
        if ( g_list_length( sxsld->toCreateList ) == 0 ) {
                w = glade_xml_get_widget( sxsld->gxml, CREATED_PG );
                gnome_druid_set_page( sxsld->sincelast_druid,
                                      GNOME_DRUID_PAGE(w) );
                return;
        }

        w = glade_xml_get_widget( sxsld->gxml, TO_CREATE_CLIST );
        gtk_clist_freeze( GTK_CLIST(w) );
        gtk_clist_clear( GTK_CLIST(w) );
        sxsld->tcl_row = 0;
        add_to_create_list_to_gui( sxsld->toCreateList, sxsld );
        gtk_clist_thaw( GTK_CLIST(w) );
}

static gboolean
to_create_next( GnomeDruidPage *druid_page,
                gpointer arg1, gpointer ud )
{
        sxSinceLastData *sxsld;
        GtkCList *cl;
        GList *tcList;
        gboolean allVarsBound;
        toCreateTuple *tct;
        Query *q, *oldQuery, *newQuery;

        sxsld = (sxSinceLastData*)ud;

        cl = GTK_CLIST(glade_xml_get_widget( sxsld->gxml, TO_CREATE_CLIST ));

        /* First: check to make sure all TCTs are 'ready' [and return if not].
         * Second: create the entries based on the variable bindings. */
        tcList = sxsld->toCreateList;
        if ( tcList == NULL ) {
                DEBUG( "No transactions to create..." );
                return FALSE;
        }

        for ( ; tcList ; tcList = tcList->next ) {
                tct = (toCreateTuple*)tcList->data;
                allVarsBound = TRUE;
                g_hash_table_foreach( tct->varBindings,
                                      andequal_numerics_set,
                                      &allVarsBound );
                if ( !allVarsBound ) {
                        char tmpBuf[GNC_D_BUF_WIDTH];
                        g_date_strftime( tmpBuf, GNC_D_WIDTH, GNC_D_FMT, tct->date );
                        /* FIXME: this should be better-presented to the user. */
                        DEBUG( "SX %s on date %s still has unbound variables.",
                               xaccSchedXactionGetName(tct->sx), tmpBuf );
                        gtk_clist_select_row( cl, tct->clistRow, 0 );
                        return TRUE;
                }
        }

        tcList = sxsld->toCreateList;
        /* At this point we can assume there are to-create transactions and
           all variables are bound. */
        g_return_val_if_fail( tcList, TRUE );

        q = xaccMallocQuery();
        gnc_suspend_gui_refresh();
        for ( ; tcList ; tcList = tcList->next ) {
                GList *l = NULL;
                GList *created = NULL;

                tct = (toCreateTuple*)tcList->data;
                create_transactions_on( tct->sx, tct->date, tct, &created );
                /* Add to the Query for that register. */
                for ( l = created; l; l = l->next ) {
                        xaccQueryAddGUIDMatch( q,
                                               (GUID*)l->data,
                                               GNC_ID_TRANS,
                                               QUERY_OR );
                }
                sxsld->createdTxnGUIDList =
                        g_list_concat( sxsld->createdTxnGUIDList, created );
        }

        DEBUG( "Done with creation; updating created ledger." );

        oldQuery = gnc_ledger_display_get_query( sxsld->created_ledger );
        newQuery = xaccQueryMerge( oldQuery, q, QUERY_AND );
        gnc_ledger_display_set_query( sxsld->created_ledger, newQuery );
        gnc_ledger_display_refresh( sxsld->created_ledger );
        xaccFreeQuery( q );

        gnc_resume_gui_refresh();

        sxsld->createdSomething = TRUE;

        return FALSE;
}

static void
finish_finish( GnomeDruidPage *druid_page,
               gpointer arg1, gpointer ud )
{
        sxSinceLastData *sxsld = (sxSinceLastData*)ud;
        sxsincelast_close_handler( sxsld );
}

static void
restore_sx_temporal_state( gpointer key,
                           gpointer value,
                           gpointer user_data )
{
        SchedXaction *sx;
        sxSinceLastData *sxsld;

        sxsld = (sxSinceLastData*)user_data;

        sx = (SchedXaction*)key;
        gnc_sx_revert_to_temporal_state_snapshot( sx, (void*)value );

        gnc_sx_destroy_temporal_state_snapshot( (void*)value );
}

static gboolean 
cancel_check( GnomeDruidPage *druid_page,
              gpointer arg1, gpointer ud )
{
        GList *l;
        sxSinceLastData *sxsld = (sxSinceLastData*)ud;
        char *lastrun_cancel_check_msg =
          _( "Cancelling the Since-Last-Run dialog "
             "will revert all changes.\n"
             "Are you sure you want to lose all "
             "Scheduled Transaction changes?" );

        DEBUG( "cancel_check" );

        if ( g_list_length( sxsld->removedList ) == 0
             && g_list_length( sxsld->createdTxnGUIDList ) == 0 ) {
                /* There's nothing to cancel, so just do so... */
                return FALSE;
        }

        if ( !gnc_verify_dialog_parented( sxsld->sincelast_window,
                                          lastrun_cancel_check_msg, TRUE ) ) {
                return TRUE;
        }

        DEBUG( "reverting SX changes...\n" );
        /* Cancel policy:
         * . deleted SXes
         *   . reborn
         * . created transactions
         *   . deleted
         *   . SXes reset
         * . auto-created transactions
         *   . deleted
         *   . SXes reset
         * . reminders -> created
         *   . Trans deleted
         *   . SXes reset
         *
         * SXes reset:
         *   . end_date || num_remain_instances
         *   . last_occur_date
         */

        gnc_suspend_gui_refresh();
        /* rebirth deleted SXes */
        DEBUG( "There are %d SXes on the 'removedList' to re-birth",
               g_list_length( sxsld->removedList ) );
        if ( g_list_length( sxsld->removedList ) > 0 ) {
                GList *sxList =
                        gnc_book_get_schedxactions( gnc_get_current_book () );
                for ( l = sxsld->removedList; l; l = l->next ) {
                        DEBUG( "re-birthing SX \"%s\"",
                               xaccSchedXactionGetName( (SchedXaction*)l->data ) );
                        sxList = g_list_append( sxList,
                                                (SchedXaction*)l->data );
                }
                gnc_book_set_schedxactions( gnc_get_current_book(),
                                            sxList );
        }
        /* delete created transactions */
        DEBUG( "length( sxsld->createdTxnGUIDList ): %d",
               g_list_length( sxsld->createdTxnGUIDList ) );
        if ( g_list_length( sxsld->createdTxnGUIDList ) > 0 ) {
                Transaction *t = NULL;
                for ( l = sxsld->createdTxnGUIDList; l; l = l->next ) {
                        t = xaccTransLookup( (GUID*)l->data,
                                             gnc_get_current_book() );
                        if ( t == NULL ) {
                                char *guidStr;
                                guidStr =
                                        guid_to_string( (GUID*)l->data );
                                PERR( "We thought we created Transaction "
                                      "\"%s\", but can't "
                                      "xaccTransLookup(...) it now.",
                                      guidStr );
                                free(guidStr);
                                break;
                        }
                        xaccTransBeginEdit( t );
                        xaccTransDestroy( t );
                        xaccTransCommitEdit( t );
                        t = NULL;
                }
        }
        /* Restore the temporal state of all SXes. 
         * This is in sxInitStates [a bunch of opaque void *'s ... which
         * should be freed when we're done to prevent a memory leak.] */
        g_hash_table_foreach( sxsld->sxInitStates,
                              restore_sx_temporal_state,
                              (gpointer)sxsld );
        g_hash_table_destroy( sxsld->sxInitStates );
        sxsld->sxInitStates = NULL;

        gnc_resume_gui_refresh();
        return FALSE;
}


static void
sxsincelast_init( sxSinceLastData *sxsld )
{
        GtkWidget *w;
        static widgetSignalHandlerTuple widgets[] = {
                { SINCELAST_DRUID, "cancel",  sxsincelast_druid_cancelled },

                { REMINDER_CLIST, "select-row",   sxsld_remind_row_toggle },
                { REMINDER_CLIST, "unselect-row", sxsld_remind_row_toggle },
                
                { TO_CREATE_CLIST, "select-row",   sxsincelast_tc_row_sel },
                { TO_CREATE_CLIST, "unselect-row", sxsincelast_tc_row_unsel },

                { SELECT_ALL_BUTTON,   "clicked",
                  sx_obsolete_select_all_clicked },
                { UNSELECT_ALL_BUTTON, "clicked",
                  sx_obsolete_unselect_all_clicked },

                { NULL, NULL, NULL }
        };

        static druidSignalHandlerTuple pages[] = {
                { WHAT_TO_DO_PG,
                  whattodo_prep, NULL, NULL,
                  NULL, cancel_check },

                { REMINDERS_PG,
                  reminders_prep, theres_no_turning_back_bang, reminders_next,
                  reminders_finish, cancel_check },

                { TO_CREATE_PG,
                  to_create_prep, theres_no_turning_back_bang, to_create_next,
                  NULL, cancel_check },

                { AUTO_CREATE_NOTIFY_PG,
                  auto_create_prep, theres_no_turning_back_bang, NULL,
                  NULL, cancel_check },

                { CREATED_PG,
                  created_prep, theres_no_turning_back_bang, NULL,
                  NULL, cancel_check },

                { OBSOLETE_PG,
                  obsolete_prep, theres_no_turning_back_bang, obsolete_next,
                  NULL, cancel_check },

                { FINISH_PG,
                  NULL, NULL, NULL,
                  finish_finish, cancel_check },

                { NULL, NULL, NULL, NULL, NULL, NULL }
        };


        gnc_register_gui_component( DIALOG_SXSINCELAST_CM_CLASS,
                                    NULL,
                                    sxsincelast_close_handler,
                                    sxsld->sincelast_window );

#if 0 /* FIXME: need to be fixed because 'show-test' removal, b0rk them all. */
        gtk_signal_connect( GTK_OBJECT(sxsld->sincelast_window), "map",
                            GTK_SIGNAL_FUNC( show_test ), sxsld );
        gtk_signal_connect( GTK_OBJECT(sxsld->sincelast_window), "realize",
                            GTK_SIGNAL_FUNC( show_test ), sxsld );
#endif /* 0 */
        gtk_signal_connect( GTK_OBJECT(sxsld->sincelast_window), "destroy",
                            GTK_SIGNAL_FUNC( sxsincelast_destroy ), sxsld );

	dialog_widgets_attach_handlers(sxsld->gxml, widgets, sxsld);
        druid_pages_attach_handlers( sxsld->gxml, pages, sxsld );

        /* set all to-create clist columns to auto-resize. */
        w = glade_xml_get_widget( sxsld->gxml, TO_CREATE_CLIST );
        clist_set_all_cols_autoresize(GTK_CLIST(w), TO_CREATE_CLIST_WIDTH);
        w = glade_xml_get_widget( sxsld->gxml, REMINDER_CLIST );
        clist_set_all_cols_autoresize(GTK_CLIST(w), REMINDER_CLIST_WIDTH);
        w = glade_xml_get_widget( sxsld->gxml, SX_OBSOLETE_CLIST );
        clist_set_all_cols_autoresize(GTK_CLIST(w), SX_OBSOLETE_CLIST_WIDTH);

        sxsld->prog = glade_xml_get_widget( sxsld->gxml, WHAT_TO_DO_PROGRESS );

        create_autoCreate_ledger( sxsld );
        create_created_ledger( sxsld );

        //reminders_page_prep( sxsld );
        gtk_widget_show_all( sxsld->sincelast_window );

        process_auto_create_list( sxsld->autoCreateList, sxsld );

        w = glade_xml_get_widget( sxsld->gxml, REMINDERS_PG );
        gnome_druid_set_page( sxsld->sincelast_druid, GNOME_DRUID_PAGE(w) );
}

static void
generate_instances( SchedXaction *sx,
                    GDate *end,
                    GDate *reminderEnd,
                    GList **instanceList,
                    GList **reminderList,
                    GList **deadList )
{
        GDate gd, *gdToReturn;
        reminderTuple *rt;
        void *seqStateData;
        char tmpBuf[GNC_D_BUF_WIDTH];

        /* Process valid next instances. */
        seqStateData = xaccSchedXactionCreateSequenceState( sx );
        gd = xaccSchedXactionGetNextInstance( sx, seqStateData );
        while ( g_date_valid(&gd)
                && g_date_compare( &gd, end ) <= 0 ) {

                g_date_strftime( tmpBuf, GNC_D_WIDTH, GNC_D_FMT, &gd );

                gdToReturn = g_date_new();
                *gdToReturn = gd;
                *instanceList = g_list_append( *instanceList, gdToReturn );

                xaccSchedXactionIncrSequenceState( sx, seqStateData );
                gd = xaccSchedXactionGetInstanceAfter( sx, &gd, seqStateData );
        }

        /* Add to dead list, or process reminder instances. */
        if ( !g_date_valid( &gd )
             && deadList ) {
                toDeleteTuple *tdt;

                tdt = g_new0( toDeleteTuple, 1 );
                tdt->sx = sx;
                tdt->endDate = g_date_new();
                *tdt->endDate = gd;
                *deadList = g_list_append( *deadList, tdt );
        } else {
                while ( g_date_valid(&gd)
                        && g_date_compare( &gd, reminderEnd ) <= 0 ) {
                        g_date_strftime( tmpBuf, GNC_D_WIDTH, GNC_D_FMT, &gd );
                        rt = g_new0( reminderTuple, 1 );
                        rt->sx         = sx;
                        rt->endDate    = g_date_new();
                        *rt->endDate   = *end;
                        rt->occurDate  = g_date_new();
                        *rt->occurDate = gd;
                        rt->isSelected = FALSE;

                        *reminderList = g_list_append( *reminderList, rt );

                        xaccSchedXactionIncrSequenceState( sx, seqStateData );
                        gd = xaccSchedXactionGetInstanceAfter( sx, &gd, seqStateData );
                }
        }
        xaccSchedXactionDestroySequenceState( seqStateData );
        seqStateData = NULL;
}

#if 0
static void
free_gdate_list_elts( gpointer data, gpointer user_data )
{
        g_date_free( (GDate*)data );
}
#endif

#if 0
static void
_free_reminderTuple_list_elts( gpointer data, gpointer user_data )
{
        /* FIXME: endDate? */
        g_date_free( ((reminderTuple*)data)->occurDate );
        g_free( (reminderTuple*)data );
}
#endif

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
        if ( tct->varBindings ) {
                g_hash_table_foreach( tct->varBindings,
                                      _free_varBindings_hash_elts,
                                      NULL );
                g_hash_table_destroy( tct->varBindings );
                tct->varBindings = NULL;
        }
        g_free( tct );
}

static void
process_auto_create_list( GList *autoCreateList, sxSinceLastData *sxsld )
{
        GList *createdGUIDs = NULL;
        GList *thisGUID;
        autoCreateTuple *act;
        gboolean autoCreateState, notifyState;
        Query *q, *dlQuery, *newQuery;
        int count;

        q = xaccMallocQuery();
        gnc_suspend_gui_refresh();
        count = 0;
        gtk_progress_configure( sxsld->prog, 0, 0, g_list_length( autoCreateList ) );
        for ( ; autoCreateList ; autoCreateList = autoCreateList->next ) {
                thisGUID = createdGUIDs = NULL;
                act = (autoCreateTuple*)autoCreateList->data;
                xaccSchedXactionGetAutoCreate( act->sx,
                                               &autoCreateState,
                                               &notifyState );
                create_transactions_on( act->sx,
                                        act->date,
                                        NULL, &createdGUIDs );

                count += g_list_length( createdGUIDs );
                gtk_progress_set_value( sxsld->prog, count );
                while (g_main_iteration(FALSE));

                sxsld->autoCreatedSomething = TRUE;
                if ( notifyState ) {
                        for ( thisGUID = createdGUIDs;
                              thisGUID;
                              (thisGUID = thisGUID->next) ) {
                                xaccQueryAddGUIDMatch( q,
                                                       (GUID*)thisGUID->data,
                                                       GNC_ID_TRANS,
                                                       QUERY_OR );
                        }
                }
                /* Save these GUIDs in case we need to 'cancel' this
                 * operation [and thus delete these transactions]. */
                sxsld->createdTxnGUIDList =
                        g_list_concat( sxsld->createdTxnGUIDList,
                                       createdGUIDs );
        }

        DEBUG( "Finished creating transactions; updating ledger" );

        dlQuery = gnc_ledger_display_get_query( sxsld->ac_ledger );
        newQuery = xaccQueryMerge( dlQuery, q, QUERY_AND );
        gnc_ledger_display_set_query( sxsld->ac_ledger, newQuery );
        xaccFreeQuery( q );

        gnc_resume_gui_refresh();
        gnc_ledger_display_refresh( sxsld->ac_ledger );
}

static void
add_to_create_list_to_gui( GList *toCreateList, sxSinceLastData *sxsld )
{
        toCreateTuple *tct;
        GtkCList *clist;
        char *rowText[3];

        if ( toCreateList == NULL )
                return;

        clist = GTK_CLIST( glade_xml_get_widget( sxsld->gxml, TO_CREATE_CLIST ) );
        for ( ; toCreateList ; toCreateList = toCreateList->next ) {
                tct = (toCreateTuple*)toCreateList->data;
                /* tct->{sx,date} are already filled in. */
                tct->clistRow = sxsld->tcl_row;
                if ( ! tct->varBindings ) {
                        tct->varBindings = g_hash_table_new( g_str_hash,
                                                             g_str_equal );
                }

                /* add to clist [ahem... register... ahem] */
                rowText[0] = xaccSchedXactionGetName( tct->sx );
                rowText[1] = g_new0( char, GNC_D_WIDTH );
                g_date_strftime( rowText[1], GNC_D_WIDTH, GNC_D_FMT, tct->date );
                sxsl_get_sx_vars( tct->sx, tct->varBindings );

                if ( g_hash_table_size( tct->varBindings ) == 0 ) {
                        rowText[2] = "y";
                } else {
                        rowText[2] = "n";
                }
                gtk_clist_insert( clist, sxsld->tcl_row, rowText );
                gtk_clist_set_row_data( clist, sxsld->tcl_row, tct );
                sxsld->tcl_row++;

                g_free( rowText[1] );
        }

        if ( sxsld->tcl_row > 0 ) {
                gtk_clist_select_row( clist, 0, 0 );
        }
}

static void
add_reminders_to_gui( GList *reminderList, sxSinceLastData *sxsld )
{
        GtkCList *clist;
        char *rowText[3];
        reminderTuple *rt;

        if ( reminderList == NULL )
                return;

        clist = GTK_CLIST( glade_xml_get_widget( sxsld->gxml,
                                                 REMINDER_CLIST ) );

        do {
                rt = (reminderTuple*)reminderList->data;
                
                rowText[0] = xaccSchedXactionGetName( rt->sx );
                rowText[1] = g_new0( gchar, GNC_D_WIDTH ); 
                g_date_strftime( rowText[1],
                                 GNC_D_WIDTH, GNC_D_FMT, rt->occurDate );
                rowText[2] = g_new0( gchar, 5 ); /* FIXME: appropriate size? */
                sprintf( rowText[2], "%d",
                         (g_date_julian(rt->occurDate)
                          - g_date_julian(rt->endDate)) );

                gtk_clist_insert( clist, sxsld->rl_row, rowText );
                gtk_clist_set_row_data( clist, sxsld->rl_row, (gpointer)rt );
                sxsld->rl_row++;
                g_free( rowText[1] );
                g_free( rowText[2] );

        } while ( (reminderList = reminderList->next) );
}

static void
processRemoveList(GList *removeList, sxSinceLastData *sxsld)
{
  GtkCList *cl;
  char *rowtext[3];
  int row;
  GString *tmp_str;
  toDeleteTuple *tdt;
  FreqSpec *fs;
  rowtext[2] = g_new0(gchar, GNC_D_BUF_WIDTH ); 

  cl = GTK_CLIST( glade_xml_get_widget( sxsld->gxml,
                                        SX_OBSOLETE_CLIST ));

  tmp_str = g_string_new(NULL);

  gtk_clist_freeze( cl );
  gtk_clist_clear( cl );
  for ( row = 0; removeList;
        row++, removeList = removeList->next ) {
          tdt = (toDeleteTuple*)removeList->data;

          rowtext[0] = xaccSchedXactionGetName( tdt->sx );

          fs = xaccSchedXactionGetFreqSpec( tdt->sx );

          xaccFreqSpecGetFreqStr( fs, tmp_str );
          rowtext[1] = tmp_str->str;

          /* FIXME: This date is the first invalid one, as opposed to the
           * last valid one. :(   Or, even better, the reason for
           * obsolesence. */
          /*g_date_strftime( rowtext[2], GNC_D_WIDTH, GNC_D_FMT, tdt->endDate ); */
          strcpy( rowtext[2], "obsolete" );

          gtk_clist_insert( cl, row, rowtext );
          gtk_clist_set_row_data(cl, row, tdt );
  }
  gtk_clist_thaw( cl );

  g_string_free(tmp_str, TRUE);
  g_free(rowtext[2]);
}

/**
 * Moves the selected reminders to the appropriate [auto-create or to-create]
 * sections of the since-last-run dialog.
 **/
static void
processSelectedReminderList( GList *goodList, sxSinceLastData *sxsld )
{
        GList *list = NULL;
        reminderTuple *rt;
        autoCreateTuple *act;
        toCreateTuple *tct;
        gboolean autoCreateOpt, notifyOpt;

        for ( ; goodList ; goodList = goodList->next ) {
                rt = (reminderTuple*)goodList->data;

                DEBUG( "Processing selected reminder \"%s\"",
                       xaccSchedXactionGetName( rt->sx ) );

                xaccSchedXactionGetAutoCreate( rt->sx,
                                               &autoCreateOpt, &notifyOpt );
                if ( autoCreateOpt ) {
                        act = g_new0( autoCreateTuple, 1 );
                        act->sx = rt->sx;
                        act->date = rt->occurDate;
                        list = g_list_append( list, act );
                        process_auto_create_list( list, sxsld );
                        g_list_free( list );
                        list = NULL;
                        sxsld->autoCreateList =
                                g_list_append( sxsld->autoCreateList, act );
                } else {
                        tct = g_new0( toCreateTuple, 1 );
                        tct->sx = rt->sx;
                        tct->date = rt->occurDate;
                        list = g_list_append( list, tct );
                        sxsld->toCreateList =
                                g_list_append( sxsld->toCreateList, tct );
                }
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
        SchedXaction *sx;
        void *sx_state;
        GDate end, endPlusReminders;
        GDate *instDate;
        gint daysInAdvance;
        gboolean autocreateState, notifyState;
        gboolean showIt;
        autoCreateTuple *act;
        toCreateTuple *tct;

        showIt = FALSE;

        instanceList = NULL;

        sxList = gnc_book_get_schedxactions( gnc_get_current_book () );

        if ( sxList == NULL ) {
                DEBUG( "No scheduled transactions to populate." );
                return FALSE;
        }

        for ( ; sxList;
              sxList = sxList->next ) {
                sx = (SchedXaction*)sxList->data;
                
                /* Store initial state of SX. */
                if ( g_hash_table_lookup( sxsld->sxInitStates, sx )
                     != NULL ) {
                        PERR( "Why are we able to find a SX initial state "
                              "hash entry for something we're seeing for "
                              "the first time?" );
                        return FALSE;
                }
                sx_state = gnc_sx_create_temporal_state_snapshot( sx );
                g_hash_table_insert( sxsld->sxInitStates,
                                     sx, sx_state );

                g_date_set_time( &end, time(NULL) );
                daysInAdvance = xaccSchedXactionGetAdvanceCreation( sx );
                g_date_add_days( &end, daysInAdvance );
                
                endPlusReminders = end;
                daysInAdvance = xaccSchedXactionGetAdvanceReminder( sx );
                g_date_add_days( &endPlusReminders, daysInAdvance );
                
                generate_instances( sx, &end,
                                    &endPlusReminders,
                                    &instanceList,
                                    &sxsld->reminderList,
                                    &sxsld->toRemoveList );

                if ( instanceList == NULL ) {
                        continue;
                }

                xaccSchedXactionGetAutoCreate( sx, &autocreateState,
                                               &notifyState );
                for ( ; instanceList; instanceList = instanceList->next ) {
                        instDate = (GDate*)instanceList->data;
                        if ( autocreateState ) {
                                act = g_new0( autoCreateTuple, 1 );
                                act->sx = sx;
                                act->date = instDate;
                                sxsld->autoCreateList =
                                        g_list_append( sxsld->autoCreateList,
                                                       act );
                        } else {
                                tct = g_new0( toCreateTuple, 1 );
                                tct->sx = sx;
                                tct->date = instDate;
                                sxsld->toCreateList =
                                        g_list_append( sxsld->toCreateList, tct );
                        }
                }
                /* Report RE:showing the dialog iff there's stuff in it to
                 * show. */
                showIt |= (g_list_length( sxsld->autoCreateList ) > 0);
                showIt |= (g_list_length( sxsld->toCreateList ) > 0);
        }

        showIt |= (g_list_length( sxsld->reminderList ) > 0);
        showIt |= (g_list_length( sxsld->toRemoveList ) > 0);

        return showIt;
}

static void
clean_sincelast_dlg( sxSinceLastData *sxsld )
{
        GtkWidget *w;

        /* . clean out to-create clist
         * . free associated memories.
         *
         * FIXME: other dlg stuff to clean?
         */
        clean_variable_table( sxsld );

        w = glade_xml_get_widget( sxsld->gxml, TO_CREATE_CLIST );
        gtk_clist_clear( GTK_CLIST(w) );
        g_list_foreach( sxsld->toCreateList, _free_toCreate_list_elts, sxsld );
        g_list_free( sxsld->toCreateList );

#if 0
                g_list_foreach( autoCreateList, free_gdate_list_elts, NULL );
                g_list_free( autoCreateList );
                autoCreateList = NULL;

		  /* We have moved the GDates over to the toCreateList in
                   * sxsld, so we don't free them here. */
                g_list_free( toCreateList );
                toCreateList = NULL;

                g_list_free( instanceList );
                instanceList = NULL;
#endif /* 0 */

        sxsld->toCreateList = NULL;
        sxsld->tcl_row = sxsld->rl_row = 0;
}

static void
sxsincelast_close_handler( gpointer ud )
{
        sxSinceLastData *sxsld = (sxSinceLastData*)ud;

        DEBUG( "sxsincelast_close_handler" );
        gtk_widget_hide( sxsld->sincelast_window );
        clean_sincelast_dlg( sxsld );
        gtk_widget_destroy( sxsld->sincelast_window );
        clean_sincelast_data( sxsld );
}

static void
andequal_numerics_set( gpointer key, gpointer value, gpointer data )
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
        num = (gnc_numeric*)gtk_object_get_data( GTK_OBJECT(e), "numeric" );
        entryText = gtk_editable_get_chars( e, 0, -1 );
        dummyVarHash = g_hash_table_new( NULL, NULL );
        /* FIXME: these debugs probably want to go into a staus bar... */
        /* FIXME: Should be using xaccParseAmount instead of parser_parse_separate_vars? */
        if ( !gnc_exp_parser_parse_separate_vars( entryText, num,
                                                  NULL, dummyVarHash ) ) {
                DEBUG( "error parsing entry \"%s\"", entryText  );
                g_free( num );
                num = NULL;
        } else if ( g_hash_table_size( dummyVarHash ) != 0 ) {
                DEBUG( "no new variables allowed in variable "
                       "bindings for expression \"%s\"", entryText );
                g_free( num );
                num = NULL;
        } else if ( gnc_numeric_check( *num ) != GNC_ERROR_OK ) {
                DEBUG( "entry \"%s\" is not "
                       "gnc_numeric-parseable", entryText );
                g_free( num );
                num = NULL;
        } else {
                DEBUG( "\"%s\" parses as \"%f\"", entryText,
                       gnc_numeric_to_double( *num ) );
        }

        g_hash_table_destroy( dummyVarHash );

        {
                gpointer maybeKey, maybeValue;

                if ( g_hash_table_lookup_extended( tct->varBindings, varName,
                                                   &maybeKey, &maybeValue ) ) {
                        g_hash_table_remove( tct->varBindings, maybeKey );
                        g_free( maybeValue );
                        /* FIXME: Does the maybeKey need to be freed? */
                }
                g_hash_table_insert( tct->varBindings, maybeKey, num );
        }

        {
                GtkCList *clist;
                gboolean allVarsBound = TRUE;

                /* If there are no un-bound variables, then set the 'ready-to-go'
                   flag to 'y'. */
                g_hash_table_foreach( tct->varBindings, andequal_numerics_set, &allVarsBound );
                clist = GTK_CLIST(glade_xml_get_widget( sxsld->gxml, TO_CREATE_CLIST ));
                gtk_clist_set_text( clist, tct->clistRow, 2, ( allVarsBound ? "y" : "n" ) );
        }
}

static void
sxsincelast_destroy( GtkObject *o, gpointer ud )
{
        sxSinceLastData *sxsld = (sxSinceLastData*)ud;

        DEBUG( "sxsincelast_destroy called\n" );
        /* appropriate place to destroy data structures */
        clean_sincelast_data( sxsld );
        gnc_ledger_display_close( sxsld->ac_ledger );
        /* FIXME: need more freeing for ac_ledger? */
        sxsld->ac_ledger = NULL;
        gnc_ledger_display_close( sxsld->created_ledger );
        /* FIXME: need more freeing for created_ledger? */
        sxsld->created_ledger = NULL;

        g_free( sxsld );
}

static gboolean
create_each_transaction_helper( Transaction *t, void *d )
{
        Transaction *newT;
        GList *sList;
        GList *osList;
        Split *split;
        kvp_frame *split_kvpf;
        kvp_value *kvp_val;
        gboolean errFlag;
        createData *createUD;
        toCreateTuple *tct;
        gnc_commodity *commonCommodity = NULL;

        errFlag = FALSE;

        /* FIXME: In general, this should [correctly] deal with errors such
           as not finding the approrpiate Accounts and not being able to
           parse the formula|credit/debit strings. */

        /* FIXME: when we copy the trans_onto_trans, we don't want to copy
           the Split's kvp_frames... */

#if 0
        DEBUG( "I'm seeing Transaction \"%s\"", xaccTransGetDescription( t ) );
#endif /* 0 */

        createUD = (createData*)d;
        tct = createUD->tct;

        newT = xaccMallocTransaction(gnc_get_current_book ());
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

#if 0
                DEBUG( "\tProcessing Split \"%s\"", xaccSplitGetMemo( split ) );
                DEBUG( "\tkvp_frame: %s\n", kvp_frame_to_string( split_kvpf ) );
#endif /* 0 */

                /* from-transaction of splits */
                /* This needs to be before the value setting [below] so the
                 * balance calculations can work. */
                {
                        GUID                *acct_guid;
                        Account                *acct;
                        /* contains the guid of the split's actual account. */
                        kvp_val = kvp_frame_get_slot_path( split_kvpf,
                                                           GNC_SX_ID,
                                                           GNC_SX_ACCOUNT,
                                                           NULL );
                        if ( kvp_val == NULL ) {
                                PERR( "Null kvp_val for account" );
                        }
                        acct_guid = kvp_value_get_guid( kvp_val );
                        acct = xaccAccountLookup( acct_guid,
                                                  gnc_get_current_book ());
#if 0
                        DEBUG( "Got account with name \"%s\"",
                                xaccAccountGetName( acct ) );
#endif /* 0 */
                        if ( commonCommodity != NULL ) {
                                if ( commonCommodity != xaccAccountGetCommodity( acct ) ) {
                                        PERR( "Common-commodity difference: old=%s, new=%s\n",
                                              gnc_commodity_get_mnemonic( commonCommodity ),
                                              gnc_commodity_get_mnemonic( xaccAccountGetCommodity( acct ) ) );
                                }
                        }
                        commonCommodity = xaccAccountGetCommodity( acct );
                        xaccAccountBeginEdit( acct );
                        xaccAccountInsertSplit( acct, split );
                        xaccAccountCommitEdit( acct );
                }


                /*commonCommodity = xaccTransGetCurrency( t );*/
                /* credit/debit formulas */
                {
                        char *str, *parseErrorLoc;
                        gnc_numeric credit_num, debit_num, final;
                        int gncn_error;

                        kvp_val = kvp_frame_get_slot_path( split_kvpf,
                                                           GNC_SX_ID,
                                                           GNC_SX_CREDIT_FORMULA,
                                                           NULL);
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
#if 0
                                DEBUG( "gnc_numeric::credit: \"%s\" -> %s [%s]",
                                       str, gnc_numeric_to_string( credit_num ),
                                       gnc_numeric_to_string( gnc_numeric_reduce( credit_num ) ) );
#endif /* 0 */
                        }
                        
                        kvp_val = kvp_frame_get_slot_path( split_kvpf,
                                                           GNC_SX_ID,
                                                           GNC_SX_DEBIT_FORMULA,
                                                           NULL);
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

#if 0
                                DEBUG( "gnc_numeric::debit: \"%s\" -> %s [%s]",
                                       str, gnc_numeric_to_string( debit_num ),
                                       gnc_numeric_to_string( gnc_numeric_reduce( debit_num ) ) );
#endif /* 0 */
                        }
                        
                        final = gnc_numeric_sub_fixed( debit_num, credit_num );
                        
                        gncn_error = gnc_numeric_check( final );
                        if ( gncn_error != GNC_ERROR_OK ) {
                                PERR( "Error %d in final gnc_numeric value", gncn_error );
                                errFlag = TRUE;
                                break;
                        }
#if 0
                        DEBUG( "gnc_numeric::final: \"%s\"",
                               gnc_numeric_to_string( final ) );
#endif /* 0 */
                        xaccSplitSetBaseValue( split, final, commonCommodity );
                }
#if 0
/* NOT [YET] USED */
                kvp_val = kvp_frame_get_slot_path( split_kvpf,
                                                   GNC_SX_ID,
                                                   GNC_SX_SHARES,
                                                   NULL);

                kvp_val = kvp_frame_get_slot_path( split_kvpf,
                                                   GNC_SX_ID,
                                                   GNC_SX_AMNT,
                                                   NULL);
#endif /* 0 */
                /* FIXME:
                 *  . We'd like to store the variable bindings, but this might be
                 *    problematic [if the formulas change in the SX] [?]
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
                xaccTransDestroy( newT );
                xaccTransCommitEdit( newT );
                return FALSE;
        }

        xaccTransCommitEdit( newT );

        if ( createUD->createdGUIDs != NULL ) {
                *createUD->createdGUIDs =
                        g_list_append( *(createUD->createdGUIDs),
                                       (gpointer)xaccTransGetGUID(newT) );
        }

        return TRUE;
}

/**
 * This should be called with the dates in increasing order, or the last call
 * will set the last occur date incorrectly.
 **/
static void
create_transactions_on( SchedXaction *sx, GDate *gd,
                        toCreateTuple *tct,
                        GList **createdGUIDs )
{
        createData createUD;
        AccountGroup *ag;
        Account *acct;
        char *id;
        gboolean createdTCT;


        {
                char tmpBuf[GNC_D_WIDTH];
                g_date_strftime( tmpBuf, GNC_D_WIDTH, GNC_D_FMT, gd );
                DEBUG( "Creating transactions on %s for %s",
                       tmpBuf, xaccSchedXactionGetName( sx ) );
        }

        if ( tct != NULL
             && g_date_compare( gd, tct->date ) != 0 ) {
                PERR( "GDate and TCT date aren't equal, "
                      "which isn't a Good Thing." );
                return;
        }

        createdTCT = FALSE;
        if ( tct == NULL ) {
                /* Create a faux tct for the creation-helper. */
                tct = g_new0( toCreateTuple, 1 );
                tct->sx = sx;
                tct->date = gd;
                tct->clistRow = -1;

                createdTCT = TRUE;
        }

        gnc_suspend_gui_refresh();
        ag = gnc_book_get_template_group( gnc_get_current_book () );
        id = guid_to_string( xaccSchedXactionGetGUID(sx) );
	if(ag && id)
	{
	  acct = xaccGetAccountFromName( ag, id );
	  if(acct)
	  {
            createUD.tct = tct;
            createUD.createdGUIDs = createdGUIDs;
	    xaccAccountForEachTransaction( acct,
					   create_each_transaction_helper,
					   /*tct*/ &createUD );
	  }
	}
	if (id)
	{
	  g_free( id );
	}
        gnc_resume_gui_refresh();
        
	xaccSchedXactionSetLastOccurDate( sx, tct->date );

        if ( createdTCT ) {
                g_free( tct );
                tct = NULL;
        }
}

static void
_hashToList( gpointer key, gpointer value, gpointer user_data )
{
        *(GList**)user_data = g_list_append( *(GList**)user_data, key );
}

static void
hash_to_sorted_list( GHashTable *hashTable, GList **gl )
{
        g_hash_table_foreach( hashTable, _hashToList, gl );
        *gl = g_list_sort( *gl, g_str_equal );
}

static void
clear_variable_numerics( gpointer key, gpointer value, gpointer data )
{
        g_free( (gnc_numeric*)value );
        g_hash_table_insert( (GHashTable*)data, key, NULL );
}

void
sxsl_get_sx_vars( SchedXaction *sx, GHashTable *varHash )
{
        GList *splitList;
        kvp_frame *kvpf;
        kvp_value *kvp_val;
        Split *s;
        char *str;

        {
                AccountGroup *ag;
                Account *acct;
                char *id;

                ag = gnc_book_get_template_group( gnc_get_current_book () );
                id = guid_to_string( xaccSchedXactionGetGUID(sx) );
                acct = xaccGetAccountFromName( ag, id );
                g_free( id );
                splitList = xaccAccountGetSplitList( acct );
        }

        if ( splitList == NULL ) {
                PINFO( "SchedXaction %s has no splits",
                       xaccSchedXactionGetName( sx ) );
                return;
        }

        for ( ; splitList ; splitList = splitList->next ) {
                s = (Split*)splitList->data;

                kvpf = xaccSplitGetSlots(s);

                kvp_val = kvp_frame_get_slot_path( kvpf,
                                                   GNC_SX_ID,
                                                   GNC_SX_CREDIT_FORMULA,
                                                   NULL);
                if ( kvp_val != NULL ) {
                        str = kvp_value_get_string( kvp_val );
                        if ( str && strlen(str) != 0 ) {
                                parse_vars_from_formula( str, varHash );
                        }
                }

                kvp_val = kvp_frame_get_slot_path( kvpf,
                                                   GNC_SX_ID,
                                                   GNC_SX_DEBIT_FORMULA,
                                                   NULL);
                if ( kvp_val != NULL ) {
                        str = kvp_value_get_string( kvp_val );
                        if ( str && strlen(str) != 0 ) {
                                parse_vars_from_formula( str, varHash );
                        }
                }
        }

        g_hash_table_foreach( varHash,
                              clear_variable_numerics,
                              (gpointer)varHash );
}

static gboolean
tct_table_entry_key_handle( GtkWidget *widget, GdkEventKey *event, gpointer ud )
{
        gnc_numeric *num;
        //gchar *entryText;
        GtkEntry *ent = NULL;
        GString *str;

        if ( (event->keyval != GDK_Tab)
             && (event->keyval != GDK_ISO_Left_Tab) ) {
                return FALSE;
        }

        /* First, deal with formulas in these cells, replacing their
         * contents with the eval'd value. */
        ent = GTK_ENTRY(widget);
        num = (gnc_numeric*)gtk_object_get_data( GTK_OBJECT(ent), "numeric" );
        str = g_string_new("");
        g_string_sprintf( str, "%0.2f", gnc_numeric_to_double( *num ) );
        gtk_entry_set_text( ent, str->str );
        g_string_free( str, TRUE );

        /* Next, deal with tab-ordering in this page...  */

        // if ( entry isn't last in table )
        //    return (normal)FALSE
        // if ( unfilled entry in this table exists )
        //    change focus to unfilled entry
        // if ( no more unfilled clist-rows )
        //    return (normal)FALSE
        // clist-select next unfilled row

        // This doesn't deal with shift-tab very well ... 
        // And there's a question of if the user will allow us to futz with
        // their tab-ordering... though it's already pretty screwed up for the
        // dynamically-changing-table anyways, so they probably won't mind
        // too much... -- jsled

        return FALSE;
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
        gint varHashSize;

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

        if ( (varHashSize = g_hash_table_size( tct->varBindings )) == 0 ) {
                PINFO( "No variables to deal with" );
                return;
        }

        varList = NULL;
        hash_to_sorted_list( tct->varBindings, &varList );
        varTable = GTK_TABLE( glade_xml_get_widget( sxsld->gxml,
                                                    VARIABLE_TABLE ) );
        gtk_table_resize( varTable, varHashSize + 1, NUM_COLS );

        tableIdx = 1;
        for ( ; varList ; varList = varList->next ) {
                GString *gstr;
		const gchar *numValueStr;
                gnc_numeric *numValue, *tmpNumValue;

                gstr = g_string_sized_new(16);
                g_string_sprintf( gstr, "%s: ", (gchar*)varList->data );
                label = gtk_label_new( gstr->str );
                gtk_label_set_justify( GTK_LABEL(label), GTK_JUSTIFY_RIGHT );
                g_string_free( gstr, TRUE );

                entry = gtk_entry_new();
                gtk_object_set_data( GTK_OBJECT(entry), "varName",
                                     varList->data );
                gtk_object_set_data( GTK_OBJECT(entry), "tct", tct );
                tmpNumValue = g_new0( gnc_numeric, 1 );
                *tmpNumValue = gnc_numeric_create( 0, 1 );
                gtk_object_set_data( GTK_OBJECT(entry), "numeric",
                                     tmpNumValue );
                if ( tableIdx == varHashSize ) {
                        /* Set a flag so we can know if we're the last row of
                         * the table. */
                        gtk_object_set_data( GTK_OBJECT(entry), "lastVisualElt",
                                             (gpointer)1 );
                }

                gtk_widget_set_usize( entry, 64, 0 );
                numValue = (gnc_numeric*)g_hash_table_lookup( tct->varBindings,
                                                              varList->data );
                if ( numValue != NULL ) {
                        numValueStr =
                                xaccPrintAmount( *numValue,
                                                 gnc_default_print_info( FALSE ) );
                        gtk_entry_set_text( GTK_ENTRY(entry), numValueStr );
                }

                /* fixme::2002.02.10 jsled testing */
                gtk_signal_connect( GTK_OBJECT(entry), "key-press-event",
                                    GTK_SIGNAL_FUNC( tct_table_entry_key_handle ),
                                    NULL );
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
        }

        gtk_widget_show_all( GTK_WIDGET(varTable) );
}

static void
clean_variable_table( sxSinceLastData *sxsld )
{
        GtkTable *table;
        GList *children, *toFree;
        GtkTableChild *child;
        
        table = GTK_TABLE( glade_xml_get_widget( sxsld->gxml,
                                                 VARIABLE_TABLE ) );
        children = table->children;
        toFree = NULL;
        if ( children == NULL ) {
                PERR( "The variable-binding table should always have at "
                      "least 2 children... something's amiss." );
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
        clean_variable_table( sxsld );
}

void
print_vars_helper( gpointer key, gpointer value, gpointer user_data )
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
        
        if ( ! gnc_exp_parser_parse_separate_vars( formula, &numeric,
                                                   &foo, varHash ) ) {
                PERR( "Error parsing at \"%s\": %s",
                        foo, gnc_exp_parser_error_string() );
                return -1;
        }
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
 *
 * Returns TRUE if there are processed, valid reminders... FALSE otherwise.
 **/
static gboolean
processed_valid_reminders_listP( sxSinceLastData *sxsld )
{
        reminderTuple *rt, *prevRT;
        char *rtName;
        gboolean overallOkFlag, okFlag, prevState;
        GList *reminderListPtr;
        GList *badList;
        GList *badRecentRun;
        GList *goodList;

        rtName = NULL;
        goodList = NULL;
        overallOkFlag = TRUE;

        okFlag = prevState = TRUE;
        badList = badRecentRun = NULL;
        rt = prevRT = NULL;

        reminderListPtr = sxsld->reminderList;

        while ( reminderListPtr != NULL ) {
                prevRT = rt;
                rt = (reminderTuple*)reminderListPtr->data;

                if ( xaccSchedXactionGetName( rt->sx ) != rtName ) {
                        if ( rtName != NULL ) {
                                /* Deal with previous sequence. */
                                overallOkFlag &=
                                        inform_or_add( reminderListPtr,
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

                reminderListPtr = reminderListPtr->next;
        }

        /* Deal with final sequence. */
        if ( rtName != NULL ) {
                overallOkFlag &= inform_or_add( sxsld->reminderList,
                                                rt, okFlag,
                                                badList, &goodList );
        }

        /* cleanup */
        g_list_free( badList );
        g_list_free( badRecentRun );

        /* Handle implications of above logic. */
        if ( !overallOkFlag ) {
                g_list_free( goodList );
                return FALSE;
        }
        if ( g_list_length( goodList ) > 0 ) {
                processSelectedReminderList( goodList, sxsld );
                g_list_free( goodList );
        }

        return TRUE;
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
create_bad_reminders_msg( gpointer data, gpointer ud )
{
        GString *msg;
        reminderTuple *rt;
        static char tmpBuf[GNC_D_BUF_WIDTH];

        rt = (reminderTuple*)data;
        msg = (GString*)ud;
        g_date_strftime( tmpBuf, GNC_D_WIDTH, GNC_D_FMT, rt->occurDate );
        g_string_sprintfa( msg, tmpBuf );
        g_string_sprintfa( msg, "\n" );
}

static gboolean
inform_or_add( GList *reminders,
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
                g_list_foreach( badList, create_bad_reminders_msg, userMsg );
                gnc_error_dialog( userMsg->str );
        }

        return okFlag;
}

static void
sx_obsolete_select_all_clicked(GtkButton *button, gpointer user_data)
{
        sxSinceLastData* sxsld = user_data;
  
        GtkCList *ob_clist = GTK_CLIST(glade_xml_get_widget(sxsld->gxml, 
                                                            SX_OBSOLETE_CLIST));
        gtk_clist_select_all( ob_clist );
}

static void
sx_obsolete_unselect_all_clicked(GtkButton *button, gpointer user_data)
{
        sxSinceLastData* sxsld = user_data;
  
        GtkCList *ob_clist = GTK_CLIST(glade_xml_get_widget(sxsld->gxml, 
                                                            SX_OBSOLETE_CLIST));
        gtk_clist_unselect_all( ob_clist );
}

static void
create_autoCreate_ledger( sxSinceLastData *sxsld )
{
        SplitRegister *splitreg;
        GtkWidget *vbox, *toolbar;
        Query *q;

        q = xaccMallocQuery();
        xaccQueryAddSingleBookMatch( q, gnc_get_current_book(), QUERY_AND );
        sxsld->ac_ledger = gnc_ledger_display_query( q,
                                                     GENERAL_LEDGER,
                                                     REG_STYLE_LEDGER );
        gnc_ledger_display_set_handlers( sxsld->ac_ledger,
                                         NULL,
                                         sxsld_ledger_get_parent );
        gnc_ledger_display_set_user_data( sxsld->ac_ledger, (gpointer)sxsld );
        splitreg = gnc_ledger_display_get_split_register( sxsld->ac_ledger );
        /* FIXME: make configurable? */
        gnucash_register_set_initial_rows( 4 );

        sxsld->ac_regWidget =
                gnc_regWidget_new( sxsld->ac_ledger,
                                   GTK_WINDOW( sxsld->sincelast_window ) );

        vbox = glade_xml_get_widget( sxsld->gxml, AUTO_CREATE_VBOX );
        toolbar = gnc_regWidget_get_toolbar( sxsld->ac_regWidget );

        gtk_box_pack_start( GTK_BOX(vbox), toolbar, FALSE, FALSE, 2 );
        gtk_box_pack_end( GTK_BOX(vbox), GTK_WIDGET(sxsld->ac_regWidget), TRUE, TRUE, 2 );

#if 0
        gtk_signal_connect( GTK_OBJECT(sxed->dialog), "activate_cursor",
                            GTK_SIGNAL_FUNC(sxe_register_record_cb), sxed );
        gtk_signal_connect( GTK_OBJECT(sxed->dialog), "redraw_all",
                            GTK_SIGNAL_FUNC(sxe_register_redraw_all_cb), sxed );

#endif /* 0 */

        /* FIXME: we should do all the happy-fun register stuff... button bar
         * controls ... popups ... */

        /* configure... */
        /* don't use double-line */
        /* FIXME */
        gnc_split_register_config(splitreg,
                                  splitreg->type, splitreg->style,
                                  FALSE);

        /* don't show present/future divider [by definition, not necessary] */
        gnc_split_register_show_present_divider( splitreg, FALSE );

        /* force a refresh */
        gnc_ledger_display_refresh( sxsld->ac_ledger );
}

static void
create_created_ledger( sxSinceLastData *sxsld )
{
        SplitRegister *splitreg;
        GtkWidget *vbox, *toolbar;
        Query *q;

        q = xaccMallocQuery();
        xaccQueryAddSingleBookMatch( q, gnc_get_current_book(), QUERY_AND );
        sxsld->created_ledger = gnc_ledger_display_query( q,
                                                          GENERAL_LEDGER,
                                                          REG_STYLE_LEDGER );
        gnc_ledger_display_set_handlers( sxsld->created_ledger,
                                         NULL,
                                         sxsld_ledger_get_parent );
        gnc_ledger_display_set_user_data( sxsld->created_ledger, (gpointer)sxsld );
        splitreg = gnc_ledger_display_get_split_register( sxsld->created_ledger );
        /* FIXME: make configurable? */
        gnucash_register_set_initial_rows( 4 );

        sxsld->created_regWidget =
                gnc_regWidget_new( sxsld->created_ledger,
                                   GTK_WINDOW( sxsld->sincelast_window ) );

        vbox = glade_xml_get_widget( sxsld->gxml, CREATED_VBOX );
        toolbar = gnc_regWidget_get_toolbar( sxsld->created_regWidget );

        gtk_box_pack_start( GTK_BOX(vbox), toolbar, FALSE, FALSE, 2 );
        gtk_box_pack_end( GTK_BOX(vbox), GTK_WIDGET(sxsld->created_regWidget), TRUE, TRUE, 2 );

#if 0
        gtk_signal_connect( GTK_OBJECT(sxed->dialog), "activate_cursor",
                            GTK_SIGNAL_FUNC(sxe_register_record_cb), sxed );
        gtk_signal_connect( GTK_OBJECT(sxed->dialog), "redraw_all",
                            GTK_SIGNAL_FUNC(sxe_register_redraw_all_cb), sxed );

#endif /* 0 */

        /* FIXME: we should do all the happy-fun register stuff... button bar
         * controls ... popups ... */

        /* configure... */
        /* don't use double-line */
        /* FIXME */
        gnc_split_register_config(splitreg,
                                  splitreg->type, splitreg->style,
                                  FALSE);

        /* don't show present/future divider [by definition, not necessary] */
        gnc_split_register_show_present_divider( splitreg, FALSE );

        /* force a refresh */
        gnc_ledger_display_refresh( sxsld->created_ledger );
}

static
gncUIWidget
sxsld_ledger_get_parent( GNCLedgerDisplay *ld )
{
        sxSinceLastData *sxsld;

        sxsld = (sxSinceLastData*)gnc_ledger_display_get_user_data( ld );
        //return (gncUIWidget)sxsld->reg->sheet->window;
        return (gncUIWidget)sxsld->sincelast_window;
}

static void
clean_sincelast_data( sxSinceLastData *sxsld )
{
        /* FIXME: much more to go, here. */
        g_list_foreach( sxsld->toCreateList, _free_toCreate_list_elts, NULL );
        if ( sxsld->toCreateList ) {
                g_list_free( sxsld->toCreateList );
                sxsld->toCreateList = NULL;
        }
}
