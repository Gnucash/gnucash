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
 *   . implement changes?
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
#include "gnc-component-manager.h"
#include "gnc-engine-util.h"
#include "gnc-exp-parser.h"
#include "gnc-numeric.h"
#include "gnc-ui-util.h"
#include "gnc-ui.h"
#include "split-register.h"
#include "gnc-ledger-display.h"
#include "gnucash-sheet.h"

#include "dialog-sxsincelast.h"

#ifdef HAVE_LANGINFO_D_FMT
#include <langinfo.h>
#endif

#define DIALOG_SXSINCELAST_CM_CLASS "dialog-sxsincelast"
#define DIALOG_SXSINCELAST_REMIND_CM_CLASS "dialog-sxsincelast-remind"
#define DIALOG_SXSINCELAST_OBSOLETE_CM_CLASS "dialog-sxsincelast-obsolete"

#define DIALOG_SXSINCELAST_GLADE_NAME "Since Last Run Druid"

#define SINCELAST_DRUID   "sincelast_druid"
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

#define TO_CREATE_CLIST_WIDTH 3
#define REMINDER_CLIST_WIDTH  3
#define SX_OBSOLETE_CLIST_WIDTH 3


#ifdef HAVE_LANGINFO_D_FMT
#  define GNC_D_FMT (nl_langinfo (D_FMT))
#else
#  define GNC_D_FMT "%Y-%m-%d"
#endif

#define GNC_D_WIDTH 25
#define GNC_D_BUF_WIDTH 26

static short module = MOD_SX;

/**
 * There are two dialogs controled in this file: the since-last-run dialog,
 * and the reminder-dialog.  They cooperate and inter-relate, so they're both
 * handled here.
 **/
typedef struct _sxSinceLastData {
        GtkWidget *druid_sincelast;
        GladeXML *gxml;
  
        GList /* <toCreateTuple*> */ *toCreateData;
        GList /* <reminderTuple*> */ *reminderList;
        GList /* <toDeleteTuple*> */ *toRemoveList;

        GNCLedgerDisplay *ac_ledger;
        GnucashRegister *reg;
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

typedef struct _toDeleteTuple {
        SchedXaction *sx;
        GDate *endDate;
        gchar *freq_info;
        gboolean isSelected;
} toDeleteTuple;

typedef struct _creation_helper_userdata {
        /* the to-create tuple */
        toCreateTuple *tct;
        /* a [pointer to a] GList to append the GUIDs of newly-created
         * transactions to, or NULL */
        GList **createdGUIDs;
} createData;

/* Next reminder clist row index to create. */
static int rl_row = 0;
/* Next to-create clist row index to create. */
static int tcl_row = 0;

static void sxsincelast_init( sxSinceLastData *sxsld );
static void create_autoCreate_gen_ledger( sxSinceLastData *sxsld );
static gncUIWidget sxsld_ledger_get_parent( GNCLedgerDisplay *ld );

static gboolean sxsincelast_populate( sxSinceLastData *sxsld );
static void sxsincelast_druid_cancelled( GnomeDruid *druid, gpointer ud );
static void sxsincelast_close_handler( gpointer ud );

static void sxsincelast_ok_clicked( GtkButton *b, gpointer ud );

static void sxsincelast_cancel_clicked( GtkButton *b, gpointer ud );
static void sxsincelast_entry_changed( GtkEditable *e, gpointer ud );

static void sxsincelast_destroy( GtkObject *o, gpointer ud );

static void create_transactions_on( SchedXaction *sx,
                                    GDate *gd,
                                    toCreateTuple *tct,
                                    GList **createdGUIDs );
static gboolean create_each_transaction_helper( Transaction *t, void *d );
static void sxsl_get_sx_vars( SchedXaction *sx, GHashTable *varHash );
static void hash_to_sorted_list( GHashTable *hashTable, GList **gl );
static void andequal_numerics_set( gpointer key,
                                   gpointer value,
                                   gpointer data );
static void print_vars_helper( gpointer key,
                               gpointer value,
                               gpointer user_data );
static void clean_sincelast_data( sxSinceLastData *sxsld );
static void clean_variable_table( sxSinceLastData *sxsld );

static void processAutoCreateList( GList *,
                                   sxSinceLastData *sxsld,
                                   SchedXaction * );
static void processToCreateList( GList *,
                                 sxSinceLastData *sxsld,
                                 SchedXaction * );
static void processReminderList( GList *,
                                 sxSinceLastData *sxsld );
static void processRemoveList( GList *,
                               sxSinceLastData *sxsld );
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
static void sx_obsolete_ok_clicked(GtkButton *button, gpointer user_data);

static void sx_obsolete_cancel_clicked(GtkButton *button, gpointer user_data);
static void sx_obsolete_row_unsel(GtkCList *clist,
				  gint row,
				  gint column,
				  GdkEventButton *event,
				  gpointer user_data);

static void sx_obsolete_close_handler(gpointer user_data);

static void sx_obsolete_select_all_clicked(GtkButton *butt, gpointer user_data);
static void sx_obsolete_unselect_all_clicked(GtkButton *button, gpointer user_data);
static void sx_obsolete_row_sel(GtkCList *clist,
				gint row,
				gint column,
				GdkEventButton *event,
				gpointer user_data);

void
gnc_ui_sxsincelast_guile_wrapper( char *foo )
{
        gnc_ui_sxsincelast_dialog_create();
}

void
gnc_ui_sxsincelast_dialog_create(void)
{
        sxSinceLastData        *sxsld = g_new0( sxSinceLastData, 1 );

        sxsld->toCreateData = sxsld->reminderList = sxsld->toRemoveList = NULL;

        sxsld->gxml = gnc_glade_xml_new( SX_GLADE_FILE,
                                         DIALOG_SXSINCELAST_GLADE_NAME );
        sxsld->druid_sincelast =
                glade_xml_get_widget( sxsld->gxml,
                                      DIALOG_SXSINCELAST_GLADE_NAME );

        sxsincelast_init( sxsld );
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
        char *pageName;
        char *signal;
        gboolean (*handlerFn)();
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
                gtk_signal_connect( GTK_OBJECT(w),
                                    handler_info[i].signal, 
                                    GTK_SIGNAL_FUNC(handler_info[i].handlerFn),
                                    sxsld);
        }
}

static void
sxsincelast_druid_cancelled( GnomeDruid *druid, gpointer ud )
{
        /* FIXME: make sure the user understands the impact of what they're
         * doing, perhaps dependent on what stage of the process they're in.
         * Close, destroy, &c. */
        DEBUG( "druid cancelled" );
}

static gboolean
theres_no_turning_back_bang( GnomeDruidPage *druid_page,
                             gpointer arg1, gpointer ud )
{
        DEBUG( "there's no turning back!!! MuHahahahhaha" );
        return TRUE;
}

static gboolean 
reminders_prep( GnomeDruidPage *druid_page,
                gpointer arg1, gpointer ud )
{
        DEBUG( "reminders_prep" );
        return FALSE;
}

static gboolean 
reminders_next( GnomeDruidPage *druid_page,
                gpointer arg1, gpointer ud )
{
        sxSinceLastData *sxsld = (sxSinceLastData*)ud;
        DEBUG( "reminders_next" );
        if ( !processed_valid_reminders_listP( sxsld ) ) {
                return TRUE;
        }
        /* next-state determination */
        return FALSE;
}

static gboolean 
reminders_back( GnomeDruidPage *druid_page,
                gpointer arg1, gpointer ud )
{
        DEBUG( "reminders_back" );
        return FALSE;
}

static gboolean 
reminders_finish( GnomeDruidPage *druid_page,
                  gpointer arg1, gpointer ud )
{
        DEBUG( "reminders_finish" );
        return FALSE;
}

static gboolean 
reminders_cancel( GnomeDruidPage *druid_page,
                  gpointer arg1, gpointer ud )
{
        DEBUG( "reminders_cancel" );
        return FALSE;
}

static gboolean
obsolete_prep( GnomeDruidPage *druid_page,
               gpointer arg1, gpointer ud )
{
        sxSinceLastData *sxsld = (sxSinceLastData*)ud;
        DEBUG( "obsolete_prep" );
        if ( g_list_length( sxsld->toRemoveList ) > 0 ) {
                processRemoveList( sxsld->toRemoveList, sxsld );
        }
        return FALSE;
}

static gboolean
obsolete_next( GnomeDruidPage *druid_page,
               gpointer arg1, gpointer ud )
{
        GList *sxList, *toDelPtr;
        GtkCList *cl;
        gint row;
        toDeleteTuple *tdt;
        sxSinceLastData *sxsld = (sxSinceLastData*)ud;

        DEBUG( "obsolete_next" );
        sxList = gnc_book_get_schedxactions( gnc_get_current_book() );
        cl = GTK_CLIST( glade_xml_get_widget( sxsld->gxml,
                                              SX_OBSOLETE_CLIST ) );

        /* toDelPtr = sxsld->toRemoveList; */
        for ( toDelPtr = cl->selection;
              toDelPtr;
              toDelPtr = toDelPtr->next ) {
                row = (gint)toDelPtr->data;
                tdt = (toDeleteTuple*)gtk_clist_get_row_data( cl, row );
                sxList = g_list_remove( sxList, tdt->sx );
                sxsld->toRemoveList =
                        g_list_remove( sxsld->toRemoveList, tdt->sx );

                xaccSchedXactionFree( tdt->sx );
                tdt->sx = NULL;
                g_free( tdt );
        }

        gnc_book_set_schedxactions( gnc_get_current_book(), sxList );

        gtk_clist_freeze( cl );
        gtk_clist_clear( cl );
        gtk_clist_thaw( cl );

        return FALSE;
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

        sxsld = (sxSinceLastData*)ud;

        cl = GTK_CLIST(glade_xml_get_widget( sxsld->gxml, TO_CREATE_CLIST ));

        /* First: check to make sure all TCTs are 'ready' [and return if not].
         * Second: create the entries based on the variable bindings. */

        tcList = sxsld->toCreateData;
        if ( tcList == NULL ) {
                DEBUG( "No transactions to create..." );
                /* FIXME: sxsincelast_close_handler( sxsld ); */
                return FALSE;
        }
        do {
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
        } while ( (tcList = tcList->next) );

        tcList = sxsld->toCreateData;
        /* At this point we can assume there are to-create transactions and
           all variables are bound. */
        g_return_val_if_fail( tcList, TRUE );
        do {
                tct = (toCreateTuple*)tcList->data;
                create_transactions_on( tct->sx, tct->date, tct, NULL );
        } while ( (tcList = tcList->next) );

        /* FXME: place in GL for review */

        return FALSE;
}

static void
sxsincelast_init( sxSinceLastData *sxsld )
{
        GtkWidget *w;
        int i;
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
                { NULL,            NULL,           NULL }
        };

        static druidSignalHandlerTuple pages[] = {
                { REMINDERS_PG, "prepare", reminders_prep },
                { REMINDERS_PG, "next",    reminders_next },
                { REMINDERS_PG, "finish",  reminders_finish },
                { REMINDERS_PG, "cancel",  reminders_cancel },

                { TO_CREATE_PG, "next", to_create_next },

                { REMINDERS_PG, "back",    theres_no_turning_back_bang },
                { AUTO_CREATE_NOTIFY_PG, "back", theres_no_turning_back_bang },
                { TO_CREATE_PG, "back", theres_no_turning_back_bang },
                { CREATED_PG, "back", theres_no_turning_back_bang },
                { OBSOLETE_PG, "back",     theres_no_turning_back_bang },

                { OBSOLETE_PG, "prepare",  obsolete_prep },
                { OBSOLETE_PG, "next",     obsolete_next },

                { NULL, NULL, NULL }
        };


        gnc_register_gui_component( DIALOG_SXSINCELAST_CM_CLASS,
                                    NULL,
                                    sxsincelast_close_handler,
                                    sxsld->druid_sincelast );

        gtk_signal_connect( GTK_OBJECT(sxsld->druid_sincelast), "destroy",
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

        rl_row = tcl_row = 0;

        create_autoCreate_gen_ledger( sxsld );

        /* FIXME: deal with nothing-to-do. */
        if ( ! sxsincelast_populate( sxsld ) ) {
                sxsincelast_close_handler( sxsld );
                return;
        }
        gtk_widget_show_all( sxsld->druid_sincelast );
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

        DEBUG( "Generating instances for \"%s\"",
               xaccSchedXactionGetName( sx ) );

        /* Process valid next instances. */
        seqStateData = xaccSchedXactionCreateSequenceState( sx );
        gd = xaccSchedXactionGetNextInstance( sx, seqStateData );
        while ( g_date_valid(&gd)
                && g_date_compare( &gd, end ) <= 0 ) {

                g_date_strftime( tmpBuf, GNC_D_WIDTH, GNC_D_FMT, &gd );
                DEBUG( "Adding instance %s", tmpBuf );

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

                PINFO( "Should be added to dead-list..." );
                tdt = g_new0( toDeleteTuple, 1 );
                tdt->sx = sx;
                tdt->endDate = g_date_new();
                *tdt->endDate = gd;
                *deadList = g_list_append( *deadList, tdt );
        } else {
                while ( g_date_valid(&gd)
                        && g_date_compare( &gd, reminderEnd ) <= 0 ) {
                        g_date_strftime( tmpBuf, GNC_D_WIDTH, GNC_D_FMT, &gd );
                        DEBUG( "Adding reminder instance %s", tmpBuf );
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
        xaccSchedXactionDestroySequenceState( sx, seqStateData );
        seqStateData = NULL;
}

static void
free_gdate_list_elts( gpointer data, gpointer user_data )
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
        Query *autoCreateQuery;
        GList *createdGUIDs = NULL;
        GList *thisGUID;
        gboolean autoCreateState, notifyState;
        char *rowText[2];

        /* get the "automagically created and notification requested"
         * register, and create the entries in it. */
        autoCreateQuery = xaccMallocQuery();
        xaccQuerySetGroup( autoCreateQuery,
                           gnc_book_get_group( gnc_get_current_book() ) );
        while ( autoCreateList ) {
                xaccSchedXactionGetAutoCreate( sx,
                                               &autoCreateState,
                                               &notifyState );
                create_transactions_on( sx, (GDate*)autoCreateList->data,
                                        NULL, &createdGUIDs );
                if ( notifyState ) {
                        thisGUID = createdGUIDs;
                        for ( ; thisGUID ; (thisGUID = thisGUID->next) ) {
                                xaccQueryAddGUIDMatch( autoCreateQuery,
                                                       (GUID*)thisGUID->data,
                                                       QUERY_OR );
                        }
                        thisGUID = createdGUIDs = NULL;
                }
                autoCreateList = autoCreateList->next;
        }

        gnc_ledger_display_set_query( sxsld->ac_ledger, autoCreateQuery );
        gnc_ledger_display_refresh( sxsld->ac_ledger );

        xaccFreeQuery( autoCreateQuery );

        g_list_free( createdGUIDs );
        createdGUIDs = NULL;
}

static void
processToCreateList( GList *toCreateList, sxSinceLastData *sxsld, SchedXaction *sx )
{
        toCreateTuple *tct;
        GtkCList *clist;
        char *rowText[3];

        if ( toCreateList == NULL )
                return;

        clist = GTK_CLIST( glade_xml_get_widget( sxsld->gxml, TO_CREATE_CLIST ) );
        do {
                tct = g_new0( toCreateTuple, 1 );
                tct->sx = sx;
                tct->date = (GDate*)toCreateList->data;
                tct->clistRow = tcl_row;
                tct->varBindings = g_hash_table_new( g_str_hash, g_str_equal );

                /* add to clist [ahem... register... ahem] */
                rowText[0] = xaccSchedXactionGetName( sx );
                rowText[1] = g_new0( char, GNC_D_WIDTH );
                g_date_strftime( rowText[1], GNC_D_WIDTH, GNC_D_FMT, tct->date );
                sxsl_get_sx_vars( sx, tct->varBindings );

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
processReminderList( GList *reminderList, sxSinceLastData *sxsld )
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

                gtk_clist_insert( clist, rl_row, rowText );
                gtk_clist_set_row_data( clist, rl_row, (gpointer)rt );
                rl_row += 1;
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
  GDate *gd;
  rowtext[2] = g_new0(gchar, GNC_D_BUF_WIDTH ); 

  cl = GTK_CLIST( glade_xml_get_widget( sxsld->gxml,
                                        SX_OBSOLETE_CLIST ));

  tmp_str = g_string_new(NULL);

  gtk_clist_freeze( cl );
  gtk_clist_clear( cl );
  for(row = 0; removeList != NULL; row++, removeList = removeList->next) {
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
          gtk_clist_set_row_data(cl, row, removeList->data );
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
        reminderTuple *rt;
        gboolean autoCreateOpt, notifyOpt;
        GList *list = NULL;

        for ( ; goodList ; goodList = goodList->next ) {
                rt = (reminderTuple*)goodList->data;

                DEBUG( "Processing selected reminder \"%s\"",
                       xaccSchedXactionGetName( rt->sx ) );

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

                g_list_free( list );
                list = NULL;
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
        GList *autoCreateList, *toCreateList;
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

        autoCreateList = toCreateList = NULL;
        autoCreateRow  = toCreateRow  = remindersRow = 0;
        instanceList = NULL;

        sxList = gnc_book_get_schedxactions( gnc_get_current_book () );

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
                
                generate_instances( sx, &end,
                                    &endPlusReminders,
                                    &instanceList,
                                    &sxsld->reminderList,
                                    &sxsld->toRemoveList );

                if ( instanceList == NULL ) {
                        break;
                }

                xaccSchedXactionGetAutoCreate( sx, &autocreateState,
                                               &notifyState );
                do {
                        instDate = (GDate*)instanceList->data;
                        if ( autocreateState ) {
                                autoCreateList =
                                        g_list_append( autoCreateList,
                                                       instDate );
                        } else {
                                toCreateList =
                                        g_list_append( toCreateList,
                                                       instDate );
                        }
                } while ( (instanceList = instanceList->next) );

                /* Report RE:showing the dialog iff there's stuff in it to
                 * show. */
                showIt |= (g_list_length( autoCreateList ) > 0);
                showIt |= (g_list_length( toCreateList ) > 0);

                processAutoCreateList( autoCreateList, sxsld, sx );
                processToCreateList  ( toCreateList,   sxsld, sx );

                g_list_foreach( autoCreateList, free_gdate_list_elts, NULL );
                g_list_free( autoCreateList );
                autoCreateList = NULL;

		  /* We have moved the GDates over to the toCreateData list in
                   * sxsld, so we don't free them here. */
                g_list_free( toCreateList );
                toCreateList = NULL;

                g_list_free( instanceList );
                instanceList = NULL;

        } while ( (sxList = sxList->next) );

        processReminderList( sxsld->reminderList, sxsld );
        showIt |= (g_list_length( sxsld->reminderList ) > 0);

        /* Defer showing the obsolete list until the end of the reminder
         * processing [as selection of reminders to create may affect the
         * list length. */
        showIt |= (g_list_length( sxsld->toRemoveList ) > 0);

        return showIt;
}

static void
clean_sincelast_dlg( sxSinceLastData *sxsld )
{
        GtkWidget *w;

        /* . clean out to-create clist
         * . free associated memories.
         */
        clean_variable_table( sxsld );

        w = glade_xml_get_widget( sxsld->gxml, TO_CREATE_CLIST );
        gtk_clist_clear( GTK_CLIST(w) );
        g_list_foreach( sxsld->toCreateData, _free_toCreate_list_elts, sxsld );
        g_list_free( sxsld->toCreateData );
        sxsld->toCreateData = NULL;
        tcl_row = 0;
}

/* 
 * FIXME: This is workable w/two dialogs, but not with three!!
 */
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
 * 
 * Fragile code alert... :( 
 **/
/* major problem... we don't know, here, whether we've been
 * called because we've been asked to close, or because the
 * remind_dialog close has called us.  This is important,
 * because it determines what we do at this point:
 *
 * 1. We were called...
 * ...thus, we clean out and raise the reminder dlg, or
 * create the obsolete dialog.
 *
 * 2. The remind-dialog-close was called, and then called us...
 * ...thus, we wait around for our interaction to complete.
 */
static void
sxsincelast_close_handler( gpointer ud )
{
        sxSinceLastData *sxsld = (sxSinceLastData*)ud;

        DEBUG( "sxsincelast_close_handler" );
        clean_sincelast_dlg( sxsld );
        gtk_widget_hide( sxsld->druid_sincelast );
        gtk_widget_destroy( sxsld->druid_sincelast );
        clean_sincelast_data( sxsld );

        /* gtk_widget_destroy( sxsld->druid_sincelast ); */
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
        gint row;
        char *rowText[2];
        toCreateTuple *tct;

        GList *tcList;
        gboolean allVarsBound;

        sxsld = (sxSinceLastData*)ud;

        cl = GTK_CLIST(glade_xml_get_widget( sxsld->gxml, TO_CREATE_CLIST ));

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
                                      andequal_numerics_set,
                                      &allVarsBound );
                if ( !allVarsBound ) {
                        char tmpBuf[GNC_D_BUF_WIDTH];
                        g_date_strftime( tmpBuf, GNC_D_WIDTH, GNC_D_FMT, tct->date );
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
                create_transactions_on( tct->sx, tct->date, tct, NULL );
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
        entryText = gtk_editable_get_chars( e, 0, -1 );
        num = g_new0( gnc_numeric, 1 );
        *num = gnc_numeric_create( 0, 1 );
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
        } else if ( gnc_numeric_zero_p( *num ) ) {
                DEBUG( "entry \"%s\" parses as '0'",
                       ( entryText ? entryText : "(null)" ) );
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
                        /* Does the maybeKey need to be freed? */
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

        DEBUG( "I'm seeing Transaction \"%s\"", xaccTransGetDescription( t ) );

        createUD = (createData*)d;
        tct = createUD->tct;

        newT = xaccMallocTransaction(gnc_get_current_session ());
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
                        kvp_val = kvp_frame_get_slot_path( split_kvpf,
                                                           GNC_SX_ID,
                                                           GNC_SX_ACCOUNT,
                                                           NULL );
                        if ( kvp_val == NULL ) {
                                PERR( "Null kvp_val for account" );
                        }
                        acct_guid = kvp_value_get_guid( kvp_val );
                        acct = xaccAccountLookup( acct_guid,
                                                  gnc_get_current_session ());
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
                                
                                DEBUG( "gnc_numeric::credit: \"%s\" -> %s [%s]",
                                       str, gnc_numeric_to_string( credit_num ),
                                       gnc_numeric_to_string( gnc_numeric_reduce( credit_num ) ) );
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
        char tmpBuf[GNC_D_BUF_WIDTH];
        gboolean createdTCT;


        {
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
_clear_variable_numerics( gpointer key, gpointer value, gpointer data )
{
        g_free( (gnc_numeric*)value );
        g_hash_table_insert( (GHashTable*)data, key, NULL );
}

static void
sxsl_get_sx_vars( SchedXaction *sx, GHashTable *varHash )
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

                ag = gnc_book_get_template_group( gnc_get_current_book () );
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
        hash_to_sorted_list( tct->varBindings, &varList );

        if ( g_hash_table_size( tct->varBindings ) == 0 ) {
                PINFO( "No variables to deal with" );
                return;
        }

        varTable = GTK_TABLE( glade_xml_get_widget( sxsld->gxml,
                                                    VARIABLE_TABLE ) );
        gtk_table_resize( varTable, 4, NUM_COLS );

        tableIdx = 1;
        do {
                GString *gstr;
		const gchar *numValueStr;
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
                numValue = (gnc_numeric*)g_hash_table_lookup( tct->varBindings,
                                                              varList->data );
                if ( numValue != NULL ) {
                        numValueStr =
                                xaccPrintAmount( *numValue,
                                                 gnc_default_print_info( FALSE ) );
                        gtk_entry_set_text( GTK_ENTRY(entry), numValueStr );
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

static void
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
                g_hash_table_foreach( varHash, print_vars_helper, NULL );
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
        /* Go through to-create list and do so...
         * . if auto-create, add to auto-create list.
         * . if to-create, add to to-create list.
         * . if need to bring up since-last dialog, do so.
         * . dismiss reminder dialog
         * . if since-last dialog is empty, close.
         */
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

#if 0
static void
sx_obsolete_row_sel(GtkCList *clist,
		    gint row,
		    gint column,
		    GdkEventButton *event,
		    gpointer user_data)
{
  sxSinceLastData *sxsld;

  GList *row_sxlentry_data;
  sxsld = (sxSinceLastData*) user_data;
  
  row_sxlentry_data = (GList *)gtk_clist_get_row_data(clist, row);

  if (g_list_position(sxsld->actual_to_remove, row_sxlentry_data) == -1)
  {
    sxsld->actual_to_remove = g_list_append(sxsld->actual_to_remove, row_sxlentry_data);
  }

  return;
}

static void
sx_obsolete_row_unsel(GtkCList *clist,
		      gint row,
		      gint column,
		      GdkEventButton *event,
		      gpointer user_data)
{
  GList *position;
  GList *row_sxlentry_data = gtk_clist_get_row_data(clist, row);
  sxSinceLastData *sxsld = user_data;
  
  
  if((position = g_list_find(sxsld->actual_to_remove, row_sxlentry_data)))
  {
    sxsld->actual_to_remove = g_list_remove_link(sxsld->actual_to_remove, position);
    g_list_free_1(position);
      
  }

  return;
}

static void
sx_obsolete_ok_clicked(GtkButton *button, gpointer user_data)
{
  sxSinceLastData *sxsld = user_data;
  SchedXaction *sx;
  GList *actual_sx_list, *actual_sx_listref, *removelist_ref;
  
  GNCBook *book = gnc_get_current_book ();

  actual_sx_list = gnc_book_get_schedxactions(book);
  for(removelist_ref = sxsld->actual_to_remove;
      removelist_ref; removelist_ref = removelist_ref->next)
  {
    actual_sx_listref = removelist_ref->data;
    
    xaccSchedXactionFree( (SchedXaction *) (actual_sx_listref->data) );
    actual_sx_list = g_list_remove_link(actual_sx_list, 
					actual_sx_listref);

    g_list_free_1(actual_sx_listref);
  }

  g_list_free(sxsld->actual_to_remove);
  sxsld->actual_to_remove = NULL;

  gnc_book_set_schedxactions(book, actual_sx_list);

  printf( "c\n" );
  gnome_dialog_close(GNOME_DIALOG(sxsld->dlg_obsolete));
}
 

static void 
sx_obsolete_close_handler(gpointer user_data)
{
        sxSinceLastData *sxsld = user_data;

        if ( sxsld->toRemoveList ) {
                /* FIXME: g_list_foreach( tdt_destroyer, ...->toRemoveList ) */
                g_list_free(sxsld->toRemoveList);
                sxsld->toRemoveList = NULL;
        }

        if ( sxsld->actual_to_remove ) {
                g_list_free(sxsld->actual_to_remove);
                sxsld->actual_to_remove = NULL;
        }
        sxsld->n_obsolete = 0;
  
        if ( sxsld->dlg_obsolete ) {
                printf( "d\n" );
                gnome_dialog_close(GNOME_DIALOG(sxsld->dlg_obsolete));
                sxsld->dlg_obsolete = NULL;
                sxsld->obsolete_displayed = FALSE;
        }
        sxsincelast_close_handler( sxsld );
}

static void
sx_obsolete_cancel_clicked(GtkButton *button, gpointer user_data)
{
  sx_obsolete_close_handler(user_data);
}
#endif /* 0 */

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
create_autoCreate_gen_ledger( sxSinceLastData *sxsld )
{
        SplitRegister *splitreg;
        GtkWidget *vbox;
        GtkWidget *toolbar;
        GtkWidget *regWidget;
        GtkWidget *popup;

        sxsld->ac_ledger = gnc_ledger_display_gl();

        gnc_ledger_display_set_handlers( sxsld->ac_ledger,
                                         NULL,
                                         sxsld_ledger_get_parent );
        gnc_ledger_display_set_user_data( sxsld->ac_ledger, (gpointer)sxsld );

        splitreg = gnc_ledger_display_get_split_register( sxsld->ac_ledger );
        /* FIXME: make configurable? */
        gnucash_register_set_initial_rows( 4 );

        regWidget = gnucash_register_new( splitreg->table );
        gnc_table_init_gui( regWidget, splitreg );
        sxsld->reg = GNUCASH_REGISTER(regWidget);
        GNUCASH_SHEET(sxsld->reg->sheet)->window =
                GTK_WIDGET( sxsld->druid_sincelast );
        
        vbox = glade_xml_get_widget( sxsld->gxml, AUTO_CREATE_VBOX );
        toolbar = gtk_label_new( "foo ... toolbar...uh... fooo" );

        gtk_box_pack_start( GTK_BOX(vbox), toolbar, FALSE, FALSE, 2 );
        gtk_widget_show_all( toolbar );
        /* FIXME: Too bad this just increases it's size w/o bound. */
        gtk_box_pack_end( GTK_BOX(vbox), regWidget, TRUE, TRUE, 2 );
        gtk_widget_show_all( regWidget );

#if 0
        gtk_signal_connect( GTK_OBJECT(sxed->dialog), "activate_cursor",
                            GTK_SIGNAL_FUNC(sxe_register_record_cb), sxed );
        gtk_signal_connect( GTK_OBJECT(sxed->dialog), "redraw_all",
                            GTK_SIGNAL_FUNC(sxe_register_redraw_all_cb), sxed );

#endif /* 0 */

        /* FIXME: we should do all the happy-fun register stuff... button bar
         * controls ... popups ... */
#if 0
        popup = schedXaction_editor_create_reg_popup( sxsld );
        gnucash_register_attach_popup( sxsld->reg, popup, sxsld );
#endif /* 0 */

        /* configure... */
        /* don't use double-line */
        gnc_split_register_config(splitreg,
                                  splitreg->type, splitreg->style,
                                  FALSE);

        /* don't show present/future divider [by definition, not necessary] */
        gnc_split_register_show_present_divider( splitreg, FALSE );

        /* force a refresh */
        gnc_ledger_display_refresh( sxsld->ac_ledger );
}

static
gncUIWidget
sxsld_ledger_get_parent( GNCLedgerDisplay *ld )
{
        sxSinceLastData *sxsld;

        sxsld = (sxSinceLastData*)gnc_ledger_display_get_user_data( ld );
        return (gncUIWidget)sxsld->reg->sheet->window;
}

static void
clean_sincelast_data( sxSinceLastData *sxsld )
{
        /* FIXME: much more to go, here. */
        g_list_foreach( sxsld->toCreateData, _free_toCreate_list_elts, NULL );
        if ( sxsld->toCreateData ) {
                g_list_free( sxsld->toCreateData );
                sxsld->toCreateData = NULL;
        }
}

#if 0
allocation: x: 15, y: 107, w: 719, h: 195, sheet_width: 195, sheet_height: 719
size_request: returning 719 x 195
size_request: returning 719 x 195

Breakpoint 2, gnucash_sheet_size_allocate (widget=0x839b708, 
    allocation=0xbfffd6c4) at gnucash-sheet.c:1013
(gdb) bt
#0  gnucash_sheet_size_allocate (widget=0x839b708, allocation=0xbfffd6c4)
    at gnucash-sheet.c:1013
#1  0x40a18ae9 in gtk_marshal_NONE__POINTER (object=0x839b708, 
    func=0x403b8b1c <gnucash_sheet_size_allocate>, func_data=0x0, 
    args=0xbfffd418) at gtkmarshal.c:193
#2  0x40a4486b in gtk_signal_real_emit (object=0x839b708, signal_id=18, 
    params=0xbfffd418) at gtksignal.c:1440
#3  0x40a42c40 in gtk_signal_emit (object=0x839b708, signal_id=18)
    at gtksignal.c:552
#4  0x40a7689b in gtk_widget_size_allocate (widget=0x839b708, 
    allocation=0xbfffd718) at gtkwidget.c:2496
#5  0x40a55652 in gtk_table_size_allocate_pass2 (table=0x839b4d0)
    at gtktable.c:1551
#6  0x40a54342 in gtk_table_size_allocate (widget=0x839b4d0, 
    allocation=0xbfffda88) at gtktable.c:832
#7  0x40a18ae9 in gtk_marshal_NONE__POINTER (object=0x839b4d0, 
    func=0x40a54288 <gtk_table_size_allocate>, func_data=0x0, args=0xbfffd7dc)
    at gtkmarshal.c:193
#8  0x40a4486b in gtk_signal_real_emit (object=0x839b4d0, signal_id=18, 
    params=0xbfffd7dc) at gtksignal.c:1440
#9  0x40a42c40 in gtk_signal_emit (object=0x839b4d0, signal_id=18)
    at gtksignal.c:552
#10 0x40a7689b in gtk_widget_size_allocate (widget=0x839b4d0, 
    allocation=0xbfffdafc) at gtkwidget.c:2496
#11 0x40a6c668 in gtk_vbox_size_allocate (widget=0x82e8e30, 
    allocation=0xbfffde48) at gtkvbox.c:329
#12 0x40a18ae9 in gtk_marshal_NONE__POINTER (object=0x82e8e30, 
    func=0x40a6c174 <gtk_vbox_size_allocate>, func_data=0x0, args=0xbfffdb9c)
    at gtkmarshal.c:193
#13 0x40a4486b in gtk_signal_real_emit (object=0x82e8e30, signal_id=18, 
    params=0xbfffdb9c) at gtksignal.c:1440
#14 0x40a42c40 in gtk_signal_emit (object=0x82e8e30, signal_id=18)
    at gtksignal.c:552
#15 0x40a7689b in gtk_widget_size_allocate (widget=0x82e8e30, 
    allocation=0xbfffdeb8) at gtkwidget.c:2496
#16 0x40a03c3b in gtk_hbox_size_allocate (widget=0x82e8de8, 
    allocation=0xbfffe204) at gtkhbox.c:275
#17 0x40a18ae9 in gtk_marshal_NONE__POINTER (object=0x82e8de8, 
    func=0x40a038e0 <gtk_hbox_size_allocate>, func_data=0x0, args=0xbfffdf58)
    at gtkmarshal.c:193
#18 0x40a4486b in gtk_signal_real_emit (object=0x82e8de8, signal_id=18, 
    params=0xbfffdf58) at gtksignal.c:1440
#19 0x40a42c40 in gtk_signal_emit (object=0x82e8de8, signal_id=18)
    at gtksignal.c:552
#20 0x40a7689b in gtk_widget_size_allocate (widget=0x82e8de8, 
    allocation=0xbfffe278) at gtkwidget.c:2496
#21 0x40a6c4e1 in gtk_vbox_size_allocate (widget=0x82e8da0, 
    allocation=0xbfffe5c4) at gtkvbox.c:273
#22 0x40a18ae9 in gtk_marshal_NONE__POINTER (object=0x82e8da0, 
    func=0x40a6c174 <gtk_vbox_size_allocate>, func_data=0x0, args=0xbfffe318)
    at gtkmarshal.c:193
#23 0x40a4486b in gtk_signal_real_emit (object=0x82e8da0, signal_id=18, 
    params=0xbfffe318) at gtksignal.c:1440
#24 0x40a42c40 in gtk_signal_emit (object=0x82e8da0, signal_id=18)
    at gtksignal.c:552
#25 0x40a7689b in gtk_widget_size_allocate (widget=0x82e8da0, 
    allocation=0xbfffe5f0) at gtkwidget.c:2496
#26 0x408afe24 in gnome_druid_page_size_allocate (widget=0x82e8d08, 
    allocation=0xbfffe960) at gnome-druid-page.c:189
#27 0x408b2148 in gnome_druid_page_standard_size_allocate (widget=0x82e8d08, 
    allocation=0xbfffe960) at gnome-druid-page-standard.c:237
#28 0x40a18ae9 in gtk_marshal_NONE__POINTER (object=0x82e8d08, 
    func=0x408b2118 <gnome_druid_page_standard_size_allocate>, func_data=0x0, 
    args=0xbfffe6b4) at gtkmarshal.c:193
#29 0x40a4486b in gtk_signal_real_emit (object=0x82e8d08, signal_id=18, 
    params=0xbfffe6b4) at gtksignal.c:1440
#30 0x40a42c40 in gtk_signal_emit (object=0x82e8d08, signal_id=18)
    at gtksignal.c:552
#31 0x40a7689b in gtk_widget_size_allocate (widget=0x82e8d08, 
    allocation=0xbfffe990) at gtkwidget.c:2496
#32 0x408ae034 in gnome_druid_size_allocate (widget=0x82f2b48, 
    allocation=0xbfffecdc) at gnome-druid.c:334
#33 0x40a18ae9 in gtk_marshal_NONE__POINTER (object=0x82f2b48, 
    func=0x408addc8 <gnome_druid_size_allocate>, func_data=0x0, 
    args=0xbfffea30) at gtkmarshal.c:193
#34 0x40a4486b in gtk_signal_real_emit (object=0x82f2b48, signal_id=18, 
    params=0xbfffea30) at gtksignal.c:1440
#35 0x40a42c40 in gtk_signal_emit (object=0x82f2b48, signal_id=18)
    at gtksignal.c:552
#36 0x40a7689b in gtk_widget_size_allocate (widget=0x82f2b48, 
    allocation=0xbfffed00) at gtkwidget.c:2496
#37 0x40a7e72a in gtk_window_size_allocate (widget=0x82f1998, 
    allocation=0xbffff04c) at gtkwindow.c:1180
#38 0x40a18ae9 in gtk_marshal_NONE__POINTER (object=0x82f1998, 
    func=0x40a7e604 <gtk_window_size_allocate>, func_data=0x0, args=0xbfffeda0)
    at gtkmarshal.c:193
#39 0x40a4486b in gtk_signal_real_emit (object=0x82f1998, signal_id=18, 
    params=0xbfffeda0) at gtksignal.c:1440
#40 0x40a42c40 in gtk_signal_emit (object=0x82f1998, signal_id=18)
    at gtksignal.c:552
#41 0x40a7689b in gtk_widget_size_allocate (widget=0x82f1998, 
    allocation=0xbffff094) at gtkwidget.c:2496
#42 0x40a7f944 in gtk_window_move_resize (window=0x82f1998) at gtkwindow.c:1750
#43 0x40a7f21e in gtk_window_check_resize (container=0x82f1998)
    at gtkwindow.c:1523
#44 0x40a18c3f in gtk_marshal_NONE__NONE (object=0x82f1998, 
    func=0x40a7f178 <gtk_window_check_resize>, func_data=0x0, args=0xbffff1c4)
    at gtkmarshal.c:312
#45 0x40a449e8 in gtk_signal_real_emit (object=0x82f1998, signal_id=63, 
    params=0xbffff1c4) at gtksignal.c:1492
#46 0x40a42c40 in gtk_signal_emit (object=0x82f1998, signal_id=63)
    at gtksignal.c:552
#47 0x409d9398 in gtk_container_check_resize (container=0x82f1998)
    at gtkcontainer.c:928
#48 0x409d908f in gtk_container_idle_sizer (data=0x0) at gtkcontainer.c:847
#49 0x40bdd948 in g_idle_dispatch (source_data=0x409d9030, 
    dispatch_time=0xbffff4f8, user_data=0x0) at gmain.c:1367
#50 0x40bdc9f6 in g_main_dispatch (dispatch_time=0xbffff4f8) at gmain.c:656
#51 0x40bdcfb1 in g_main_iterate (block=1, dispatch=1) at gmain.c:877
#52 0x40bdd129 in g_main_run (loop=0x83903e8) at gmain.c:935
#53 0x40a1748a in gtk_main () at gtkmain.c:524
#54 0x403fedd8 in gnc_ui_start_event_loop () at top-level.c:676
#55 0x4001b7c9 in gwrap_gnc_ui_start_event_loop () at gw-gnc.c:257
#56 0x40b1bcb8 in scm_deval (x=1088180304, env=1088218144) at eval.c:2636
#57 0x40b19df3 in scm_deval (x=1088180272, env=1088218144) at eval.c:1954
#58 0x40b19df3 in scm_deval (x=1088180032, env=1088218144) at eval.c:1954
#59 0x40b1d885 in scm_dapply (proc=1088179888, arg1=10612, args=1088218344)
    at eval.c:3473
#60 0x40b18585 in scm_apply (proc=1088179864, arg1=10612, args=10612)
    at eval.c:3283
#61 0x40b23cfe in gh_call0 (proc=1088179864) at gh_funcs.c:150
#62 0x40424de1 in gnucash_main_helper (argc=1, argv=0xbffff9b4)
    at gnucash.c:125
#63 0x40b23dc3 in gh_launch_pad (closure=0x40424bb0, argc=1, argv=0xbffff9b4)
    at gh_init.c:60
#64 0x40b270a2 in invoke_main_func (body_data=0xbffff8f8) at init.c:625
#65 0x40b4834b in scm_internal_lazy_catch (tag=9076, 
    body=0x40b2707c <invoke_main_func>, body_data=0xbffff8f8, 
    handler=0x40b4866c <scm_handle_by_message>, handler_data=0x0)
    at throw.c:283
#66 0x40b2705c in scm_boot_guile_1 (base=0xbffff8f4, closure=0xbffff8f8)
    at init.c:600
#67 0x40b26d8d in scm_boot_guile (argc=1, argv=0xbffff9b4, 
    main_func=0x40b23da8 <gh_launch_pad>, closure=0x40424bb0) at init.c:443
#68 0x40b23df1 in gh_enter (argc=1, argv=0xbffff9b4, 
    c_main_prog=0x40424bb0 <gnucash_main_helper>) at gh_init.c:70
#69 0x40424e94 in gnc_main (argc=1, argv=0xbffff9b4) at gnucash.c:176
#70 0x8048c36 in main (argc=1, argv=0xbffff9b4) at gnc-main.c:30
#71 0x40c4f9cb in __libc_start_main (main=0x8048c28 <main>, argc=1, 
    argv=0xbffff9b4, init=0x8048aec <_init>, fini=0x8048c6c <_fini>, 
    rtld_fini=0x4000ae60 <_dl_fini>, stack_end=0xbffff9ac)
    at ../sysdeps/generic/libc-start.c:92
(gdb) c
Continuing.
allocation: x: 15, y: 107, w: 727, h: 199, sheet_width: 195, sheet_height: 719

Breakpoint 2, gnucash_sheet_size_allocate (widget=0x839b708, 
    allocation=0xbfffec64) at gnucash-sheet.c:1013
(gdb) bt
#0  gnucash_sheet_size_allocate (widget=0x839b708, allocation=0xbfffec64)
    at gnucash-sheet.c:1013
#1  0x40a18ae9 in gtk_marshal_NONE__POINTER (object=0x839b708, 
    func=0x403b8b1c <gnucash_sheet_size_allocate>, func_data=0x0, 
    args=0xbfffe9b8) at gtkmarshal.c:193
#2  0x40a4486b in gtk_signal_real_emit (object=0x839b708, signal_id=18, 
    params=0xbfffe9b8) at gtksignal.c:1440
#3  0x40a42c40 in gtk_signal_emit (object=0x839b708, signal_id=18)
    at gtksignal.c:552
#4  0x40a7689b in gtk_widget_size_allocate (widget=0x839b708, 
    allocation=0xbfffecb8) at gtkwidget.c:2496
#5  0x40a55652 in gtk_table_size_allocate_pass2 (table=0x839b4d0)
    at gtktable.c:1551
#6  0x40a54342 in gtk_table_size_allocate (widget=0x839b4d0, 
    allocation=0xbffff028) at gtktable.c:832
#7  0x40a18ae9 in gtk_marshal_NONE__POINTER (object=0x839b4d0, 
    func=0x40a54288 <gtk_table_size_allocate>, func_data=0x0, args=0xbfffed7c)
    at gtkmarshal.c:193
#8  0x40a4486b in gtk_signal_real_emit (object=0x839b4d0, signal_id=18, 
    params=0xbfffed7c) at gtksignal.c:1440
#9  0x40a42c40 in gtk_signal_emit (object=0x839b4d0, signal_id=18)
    at gtksignal.c:552
#10 0x40a7689b in gtk_widget_size_allocate (widget=0x839b4d0, 
    allocation=0x839b4f0) at gtkwidget.c:2496
#11 0x409d9640 in gtk_container_resize_children (container=0x82f1998)
    at gtkcontainer.c:1087
#12 0x40a7fa8c in gtk_window_move_resize (window=0x82f1998) at gtkwindow.c:1856
#13 0x40a7f21e in gtk_window_check_resize (container=0x82f1998)
    at gtkwindow.c:1523
#14 0x40a18c3f in gtk_marshal_NONE__NONE (object=0x82f1998, 
    func=0x40a7f178 <gtk_window_check_resize>, func_data=0x0, args=0xbffff1c4)
    at gtkmarshal.c:312
#15 0x40a449e8 in gtk_signal_real_emit (object=0x82f1998, signal_id=63, 
    params=0xbffff1c4) at gtksignal.c:1492
#16 0x40a42c40 in gtk_signal_emit (object=0x82f1998, signal_id=63)
    at gtksignal.c:552
#17 0x409d9398 in gtk_container_check_resize (container=0x82f1998)
    at gtkcontainer.c:928
#18 0x409d908f in gtk_container_idle_sizer (data=0x0) at gtkcontainer.c:847
#19 0x40bdd948 in g_idle_dispatch (source_data=0x409d9030, 
    dispatch_time=0xbffff4f8, user_data=0x0) at gmain.c:1367
#20 0x40bdc9f6 in g_main_dispatch (dispatch_time=0xbffff4f8) at gmain.c:656
#21 0x40bdcfb1 in g_main_iterate (block=1, dispatch=1) at gmain.c:877
#22 0x40bdd129 in g_main_run (loop=0x83903e8) at gmain.c:935
#23 0x40a1748a in gtk_main () at gtkmain.c:524
#24 0x403fedd8 in gnc_ui_start_event_loop () at top-level.c:676
#25 0x4001b7c9 in gwrap_gnc_ui_start_event_loop () at gw-gnc.c:257
#26 0x40b1bcb8 in scm_deval (x=1088180304, env=1088218144) at eval.c:2636
#27 0x40b19df3 in scm_deval (x=1088180272, env=1088218144) at eval.c:1954
#28 0x40b19df3 in scm_deval (x=1088180032, env=1088218144) at eval.c:1954
#29 0x40b1d885 in scm_dapply (proc=1088179888, arg1=10612, args=1088218344)
    at eval.c:3473
#30 0x40b18585 in scm_apply (proc=1088179864, arg1=10612, args=10612)
    at eval.c:3283
#31 0x40b23cfe in gh_call0 (proc=1088179864) at gh_funcs.c:150
#32 0x40424de1 in gnucash_main_helper (argc=1, argv=0xbffff9b4)
    at gnucash.c:125
#33 0x40b23dc3 in gh_launch_pad (closure=0x40424bb0, argc=1, argv=0xbffff9b4)
    at gh_init.c:60
#34 0x40b270a2 in invoke_main_func (body_data=0xbffff8f8) at init.c:625
#35 0x40b4834b in scm_internal_lazy_catch (tag=9076, 
    body=0x40b2707c <invoke_main_func>, body_data=0xbffff8f8, 
    handler=0x40b4866c <scm_handle_by_message>, handler_data=0x0)
    at throw.c:283
#36 0x40b2705c in scm_boot_guile_1 (base=0xbffff8f4, closure=0xbffff8f8)
    at init.c:600
#37 0x40b26d8d in scm_boot_guile (argc=1, argv=0xbffff9b4, 
    main_func=0x40b23da8 <gh_launch_pad>, closure=0x40424bb0) at init.c:443
#38 0x40b23df1 in gh_enter (argc=1, argv=0xbffff9b4, 
    c_main_prog=0x40424bb0 <gnucash_main_helper>) at gh_init.c:70
#39 0x40424e94 in gnc_main (argc=1, argv=0xbffff9b4) at gnucash.c:176
#40 0x8048c36 in main (argc=1, argv=0xbffff9b4) at gnc-main.c:30
#41 0x40c4f9cb in __libc_start_main (main=0x8048c28 <main>, argc=1, 
    argv=0xbffff9b4, init=0x8048aec <_init>, fini=0x8048c6c <_fini>, 
    rtld_fini=0x4000ae60 <_dl_fini>, stack_end=0xbffff9ac)
    at ../sysdeps/generic/libc-start.c:92
(gdb) quit
The program is running.  Exit anyway? (y or n) y

Debugger finished
#endif /* 0 */
