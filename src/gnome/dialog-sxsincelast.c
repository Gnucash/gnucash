/********************************************************************\
 * dialog-sxsincelast.c - "since last run" dialog.                  *
 * Copyright (c) 2001,2002 Joshua Sled <jsled@asynchronous.org>     *
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
 *   . backed by: sxsld->autoCreateList
 * . Page 3: to-create variable bindings
 *   . backed by: sxsld->toCreateData [s/Data/List/]
 * . Page 4: created ledger
 *   . backed by: sxsld->createdList
 * . Page 5: obsolete list
 *   . backed by: sxsld->toRemoveList
 *
 * Detail regarding 'back' processing support.
 * . reminders
 *   . selected
 *     . <standard policy>
 *   . unselected
 *     . if auto-created   : delete
 *     . if to-create      : remove
 * . auto-create           : display
 * . to-create variable bindings
 *   . if bindings changed : reeval/fill credit/debit cells
 *   . if made incomplete  : delete transaction
 * . created               : display
 * . obsolete              : select status [easy]
 **/

#include "config.h"

#include <limits.h>
#include <gnome.h>
#include <glib.h>

#include "Account.h"
#include "Group.h"
#include "Query.h"
#include "QueryNew.h"
#include "SchedXaction.h"
#include "Transaction.h"
#include "Scrub.h"
#include "SX-book.h"
#include "SX-book-p.h"
#include "dialog-utils.h"
#include "finvar.h"
#include "gnc-book.h"
#include "gnc-date.h"
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
#include "gnc-split-reg.h"
#include "window-register.h"
#include "messages.h"

#include "dialog-sxsincelast.h"
#include "dialog-scheduledxaction.h"

#ifdef HAVE_LANGINFO_D_FMT
#include <langinfo.h>
#endif

#define DIALOG_SXSINCELAST_CM_CLASS "dialog-sxsincelast"
#define DIALOG_SXSINCELAST_REMIND_CM_CLASS "dialog-sxsincelast-remind"
#define DIALOG_SXSINCELAST_OBSOLETE_CM_CLASS "dialog-sxsincelast-obsolete"

#define DIALOG_SXSINCELAST_GLADE_NAME "Since Last Run Druid"
#define SXSLD_DRUID_GLADE_NAME "sincelast_druid"
#define SXSLD_WIN_PREFIX "sx_sincelast_win"

#define SINCELAST_DRUID   "sincelast_druid"
#define WHAT_TO_DO_PG "what_to_do"
#define REMINDERS_PG "reminders_page"
#define AUTO_CREATE_NOTIFY_PG "auto_create_notify_page"
#define TO_CREATE_PG "to_create_page"
#define CREATED_PG "created_page"
#define OBSOLETE_PG "obsolete_page"

#define SX_OBSOLETE_CLIST "sx_obsolete_clist"
#define TO_CREATE_LIST "to_create_list"
#define REMINDER_LIST  "reminders_list"
#define SX_GLADE_FILE "sched-xact.glade"

#define TO_CREATE_STATUS "to_create_status"

#define SELECT_ALL_BUTTON "select_all_button"
#define UNSELECT_ALL_BUTTON "unselect_all_button"
#define OK_BUTTON "ok_button"
#define CANCEL_BUTTON "cancel_button"
#define VARIABLE_TABLE "variables_table"
#define AUTO_CREATE_VBOX "ac_vbox"
#define TO_CREATE_TXN_REG_FRAME "to_create_txn_reg_frame"
#define CREATED_VBOX "created_vbox"
#define WHAT_TO_DO_VBOX "what_to_do_vbox"
#define WHAT_TO_DO_PROGRESS "creation_progress"
#define SX_DISPOSITION_OPT "disposition_opt"

#define TO_CREATE_LIST_WIDTH 2
#define REMINDER_LIST_WIDTH  3
#define SX_OBSOLETE_CLIST_WIDTH 3

#define COERCE_VOID_TO_GBOOLEAN(x) ((gboolean)(*#x))

#define IGNORE_TEXT         "Ignored"
#define POSTPONE_TEXT       "Postponed"
#define READY_TEXT          "Ready to create"
#define NEEDS_BINDINGS_TEXT "Needs values for variables"

static short module = MOD_SX;

/**
 * Directions for {forward,back}-page determining.
 * @see gnc_sxsld_get_appropriate_page
 **/
typedef enum {
        FORWARD, BACK
} Direction;

/**
 * The states a to-be-created SX can be in...
 * TO_CREATE   : The SX is ready to be created, depending on variable-binding
 *               requirements.
 * IGNORE      : Drop the SX on the floor forever.
 * POSTPONE    : Bring this SX up in the future, but we're not going to
 *               create it right now.
 * [MAX_STATE] : The maximum real value.
 * UNDEF       : Only used for prevState, to indicate that we haven't
 *               processed this instance, yet.
 **/
typedef enum {
        TO_CREATE,
        IGNORE,
        POSTPONE,
        MAX_STATE,
        UNDEF
} ToCreateState;

typedef struct toCreateTuple_ {
        SchedXaction *sx;
        GList /* <toCreateInstance*> */ *instanceList;
} toCreateTuple;

typedef struct toCreateInstance_ {
        GDate *date;
        GHashTable *varBindings;
        void *sxStateData;
        GtkCTreeNode *node;
        toCreateTuple *parentTCT;
        /* A list of the GUIDs of transactions generated from this TCI [if
         * any]; this will always be a subset of the
         * sxsld->createdTxnGUIDList. */
        GList /* <GUID*> */ *createdTxnGUIDs;
        gboolean dirty;
        /** How this was, originally -- for revert processing. **/
        ToCreateState origState;
        /** How the user would currently like to process this instance
         * [within the druid]. */
        ToCreateState state;
        /** How we've previously processed this instance [within the druid]. */
        ToCreateState prevState;
} toCreateInstance;

/**
 * A tuple of an SX and any upcoming reminders.
 **/
typedef struct reminderTuple_ {
        SchedXaction *sx;
        GList /* <reminderInstanceTuple*> */ *instanceList;
} reminderTuple;

/**
 * An reminder instance of the containing SX.
 **/
typedef struct reminderInstanceTuple_ {
        GDate	*endDate;
        GDate	*occurDate;
        void    *sxStateData;
        gboolean isSelected;
        reminderTuple *parentRT;
        toCreateInstance *resultantTCI;
} reminderInstanceTuple;

typedef struct toDeleteTuple_ {
        SchedXaction *sx;
        GDate *endDate;
        gboolean isSelected;
} toDeleteTuple;

typedef struct creation_helper_userdata_ {
        /* the to-create tuple */
        toCreateInstance *tci;
        /* a [pointer to a] GList to append the GUIDs of newly-created
         * Transactions to, or NULL */
        GList **createdGUIDs;
} createData;

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
        GtkStatusbar *toCreateFormula;
        guint formulaCtxId;
        GtkStatusbar *toCreateStatus;
        guint statusCtxId;

        /* The currently-selected to-create instance. */
        toCreateInstance *curSelTCI;

        /* Multi-stage processing-related stuff... */
        GList /* <toCreateTuple*> */ *autoCreateList;
        GList /* <toCreateTuple*> */ *toCreateList;
        GList /* <reminderTuple*> */ *reminderList;
        GList /* <toDeleteTuple*> */ *toRemoveList;

        /********** "Cancel"-related stuff... **********/
        
        /** A HashTable of SX mapped to initial temporal data. */
        GHashTable /* <SchedXaction*,void*> */ *sxInitStates;

        /** The list of the GUIDs of _all_ transactions we've created. */
        GList /* <GUID*> */            *createdTxnGUIDList;

        /* The count of selected reminders. */
        gint remindSelCount;

        /* The count of auto-created transactions. */
        gint autoCreatedCount;

        GNCLedgerDisplay *ac_ledger;
        GNCSplitReg *ac_gsr;

        GNCLedgerDisplay *created_ledger;
        GNCSplitReg *created_gsr;

        GNCLedgerDisplay *to_create_ledger;
        GNCSplitReg *to_create_gsr;

} sxSinceLastData;

static void sxsincelast_init( sxSinceLastData *sxsld );
static void create_autoCreate_ledger( sxSinceLastData *sxsld );
static void create_created_ledger( sxSinceLastData *sxsld );
static void create_to_create_ledger( sxSinceLastData *sxsld );
static void gnc_sxsld_commit_ledgers( sxSinceLastData *sxsld );

static gint sxsincelast_populate( sxSinceLastData *sxsld );
static void sxsincelast_druid_cancelled( GnomeDruid *druid, gpointer ud );
static void sxsincelast_close_handler( gpointer ud );

static GnomeDruidPage* gnc_sxsld_get_appropriate_page( sxSinceLastData *sxsdl,
                                                       GnomeDruidPage *from,
                                                       Direction dir );
static gboolean gnc_sxsld_wtd_appr( sxSinceLastData *sxsld );
static gboolean gnc_sxsld_remind_appr( sxSinceLastData *sxsld );
static gboolean gnc_sxsld_tocreate_appr( sxSinceLastData *sxsld );
static gboolean gnc_sxsld_autocreate_appr( sxSinceLastData *sxsld );
static gboolean gnc_sxsld_created_appr( sxSinceLastData *sxsld );
static gboolean gnc_sxsld_obsolete_appr( sxSinceLastData *sxsld );

static void sxsincelast_entry_changed( GtkEditable *e, gpointer ud );
static void sxsincelast_destroy( GtkObject *o, gpointer ud );
static void sxsincelast_save_size( sxSinceLastData *sxsld );
static void create_transactions_on( SchedXaction *sx,
                                    GDate *gd,
                                    toCreateInstance *tci,
                                    GList **createdGUIDs );
static gboolean create_each_transaction_helper( Transaction *t, void *d );
/* External for what reason ... ? */
void sxsl_get_sx_vars( SchedXaction *sx, GHashTable *varHash );
static void hash_to_sorted_list( GHashTable *hashTable, GList **gl );
static void andequal_numerics_set( gpointer key,
                                   gpointer value,
                                   gpointer data );
/* External for bad reasons, I think...? */
void print_vars_helper( gpointer key,
                        gpointer value,
                        gpointer user_data );
static void clean_sincelast_data( sxSinceLastData *sxsld );
static void clean_variable_table( sxSinceLastData *sxsld );

static void process_auto_create_list( GList *, sxSinceLastData *sxsld );
static void add_to_create_list_to_gui( GList *, sxSinceLastData *sxsld );
static void add_reminders_to_gui( GList *, sxSinceLastData *sxsld );
static void add_dead_list_to_gui( GList *, sxSinceLastData *sxsld );
static void processSelectedReminderList( GList *, sxSinceLastData * );

static void sxsincelast_tc_row_sel( GtkCTree *ct,
                                    GList *nodelist,
                                    gint column,
                                    gpointer user_data);

static void sxsincelast_tc_row_unsel( GtkCTree *ct,
                                      GList *nodelist,
                                      gint column,
                                      gpointer user_data);

static void sxsld_remind_row_toggle( GtkCTree *ct, GList *node,
                                     gint column, gpointer user_data );
static void sxsld_obsolete_row_toggle( GtkCList *cl, gint row, gint col,
                                       GdkEventButton *event, gpointer ud );

static void sxsld_disposition_changed( GtkMenuShell *b, gpointer d );
static void sxsld_set_sensitive_tci_controls( sxSinceLastData *sxsld,
                                              gboolean sensitive );

static void gnc_sxsld_revert_reminders( sxSinceLastData *sxsld,
                                        GList *toRevertList );
static gboolean processed_valid_reminders_listP( sxSinceLastData *sxsld );
static void create_bad_reminders_msg( gpointer data, gpointer ud );
static gboolean inform_or_add( sxSinceLastData *sxsld, reminderTuple *rt, gboolean okFlag,
                               GList *badList, GList **goodList );

static void sx_obsolete_select_all_clicked( GtkButton *button,
                                            gpointer user_data );
static void sx_obsolete_unselect_all_clicked( GtkButton *button,
                                              gpointer user_data );

static void gnc_sxsld_free_tci( toCreateInstance *tci );
static void gnc_sxsld_free_toCreateTuple_list( GList *l );
static void gnc_sxsld_free_sxState( gpointer key,
                                    gpointer value,
                                    gpointer userdata );
static void gnc_sxsld_free_entry_numeric( GtkObject *o, gpointer ud );

static gint sxsld_process_to_create_instance( sxSinceLastData *sxsld,
                                              toCreateInstance *tci );
static void sxsld_revert_to_create_txns( sxSinceLastData *sxsld,
                                         toCreateInstance *tci );
static gint sxsld_create_to_create_txns( sxSinceLastData *sxsld,
                                         toCreateInstance *tci );
static gint sxsld_get_future_created_txn_count( sxSinceLastData *sxsld );

/**
 * Used to wrap for the book-open hook, where the book filename is given.
 **/
void
gnc_ui_sxsincelast_guile_wrapper( char *bookfile )
{
  gint ret;
  ret = gnc_ui_sxsincelast_dialog_create();
  if ( ret < 0 ) {
    gnc_info_dialog
      (NULL,
       ngettext 
       ("There are no Scheduled Transactions to be entered at this time.\n"
	"(%d transaction automatically created)",
	"There are no Scheduled Transactions to be entered at this time.\n"
	"(%d transactions automatically created)",
	-(ret)),
       -(ret));
  }
}

static gboolean
show_handler (const char *class, gint component_id,
	      gpointer user_data, gpointer iter_data)
{
        GtkWidget *window = user_data;

        if (!window)
                return(FALSE);
        gtk_window_present (GTK_WINDOW(window));
        return(TRUE);
}

/**
 * @return The magnitude of the return value is the number of auto-created,
 * no-notification scheduled transactions created.  This value is positive if
 * there are additionally other SXes which need user interaction and the
 * Druid has been displayed, or negative if there are not, and no Druid
 * window was realized.  In the case where there the dialog has been
 * displayed but no auto-create-no-notify transactions have been created,
 * INT_MAX [limits.h] is returned.  0 is treated as negative, with no
 * transactions created and no dialog displayed.  The caller can use this
 * value as appropriate to inform the user.
 *
 * [e.g., for book-open-hook: do nothing; for menu-selection: display an info
 *  dialog stating there's nothing to do.]
 **/
gint
gnc_ui_sxsincelast_dialog_create()
{
        int autoCreateCount;
        sxSinceLastData        *sxsld;

	if (gnc_forall_gui_components (DIALOG_SXSINCELAST_CM_CLASS,
				       show_handler, NULL))
		return 0;


	sxsld = g_new0( sxSinceLastData, 1 );

        sxsld->toCreateList = sxsld->reminderList = sxsld->toRemoveList = NULL;
        sxsld->sxInitStates = g_hash_table_new( g_direct_hash, g_direct_equal );

        autoCreateCount = sxsincelast_populate( sxsld );
        if ( autoCreateCount <= 0 ) {
                g_free( sxsld );
                return autoCreateCount;
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
        return autoCreateCount;
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

        gtk_widget_hide( sxsld->sincelast_window );
        sxsincelast_close_handler( sxsld );
}

/**
 * Using the specified direction, gets the next appropriate page.  Returns
 * NULL if there is no approrpiate page to go to.
 **/
static
GnomeDruidPage*
gnc_sxsld_get_appropriate_page( sxSinceLastData *sxsld,
                                GnomeDruidPage *from,
                                Direction dir )
{
        static struct {
                gchar *pageName;
                gboolean (*pageAppropriate)( sxSinceLastData *sxsld );
        } pages[] = {
                { WHAT_TO_DO_PG,         gnc_sxsld_wtd_appr },
                { REMINDERS_PG,          gnc_sxsld_remind_appr },
                { AUTO_CREATE_NOTIFY_PG, gnc_sxsld_autocreate_appr },
                { TO_CREATE_PG,          gnc_sxsld_tocreate_appr },
                { CREATED_PG,            gnc_sxsld_created_appr },
                { OBSOLETE_PG,           gnc_sxsld_obsolete_appr },
                { NULL,                  NULL }
        };
        int modifier;
        int cur;
        GtkWidget *pg;

        pg = NULL;
        /* get the current page index via lame linear search. */
        for ( cur = 0; pages[cur].pageName != NULL; cur++ ) {
                pg = glade_xml_get_widget( sxsld->gxml, pages[cur].pageName );
                if ( GTK_WIDGET(from) == pg ) {
                        break;
                }
        }
        g_assert( pages[cur].pageName != NULL );

        modifier = ( dir == FORWARD ? 1 : -1 );
        /* Find the approrpriate "next" page; start trying the first possible
         * "next" page. */
        cur += modifier;
        while ( cur >= 0
                && pages[cur].pageName != NULL
                && !(*pages[cur].pageAppropriate)( sxsld ) ) {
                cur += modifier;
        }

        if ( cur < 0
             || pages[cur].pageName == NULL ) {
                return NULL;
        }
        return GNOME_DRUID_PAGE( glade_xml_get_widget( sxsld->gxml,
                                                       pages[cur].pageName ) );
}

static
gboolean
gnc_sxsld_wtd_appr( sxSinceLastData *sxsld )
{
        /* It's never appropriate to return here. */
        return FALSE;
}

static
gboolean
gnc_sxsld_remind_appr( sxSinceLastData *sxsld )
{
        return (g_list_length( sxsld->reminderList ) != 0);
}

static
gboolean
gnc_sxsld_tocreate_appr( sxSinceLastData *sxsld )
{
        return (g_list_length( sxsld->toCreateList ) != 0);
}

static
gboolean
gnc_sxsld_autocreate_appr( sxSinceLastData *sxsld )
{
        return (sxsld->autoCreatedCount > 0);
}

static
gboolean
gnc_sxsld_created_appr( sxSinceLastData *sxsld )
{
        return ((g_list_length(sxsld->createdTxnGUIDList)
                 - sxsld->autoCreatedCount) > 0);
}

static
gboolean
gnc_sxsld_obsolete_appr( sxSinceLastData *sxsld )
{
        return (g_list_length( sxsld->toRemoveList ) != 0);
}

static
gboolean
gen_back( GnomeDruidPage *druid_page,
          gpointer arg1, gpointer ud )
{
        GnomeDruidPage *gdp;
        sxSinceLastData *sxsld = (sxSinceLastData*)ud;

        if ( !(gdp = gnc_sxsld_get_appropriate_page( sxsld, druid_page, BACK )) ) {
                DEBUG( "No appropriate page to go to." );
                return TRUE;
        }
        gnome_druid_set_page( sxsld->sincelast_druid, gdp );
        return TRUE;
}

static
gboolean
gen_next( GnomeDruidPage *druid_page,
          gpointer arg1, gpointer ud )
{
        GnomeDruidPage *gdp;
        sxSinceLastData *sxsld = (sxSinceLastData*)ud;

        if ( !(gdp = gnc_sxsld_get_appropriate_page( sxsld, druid_page, FORWARD )) ) {
                DEBUG( "No appropriate page to go to." );
                return TRUE;
        }
        gnome_druid_set_page( sxsld->sincelast_druid, gdp );
        return TRUE;
}

static void
whattodo_prep( GnomeDruidPage *druid_page,
               gpointer arg1, gpointer ud )
{
}

static
void 
reminders_prep( GnomeDruidPage *druid_page,
                gpointer arg1, gpointer ud )
{
        GtkWidget *w;
        sxSinceLastData *sxsld = (sxSinceLastData*)ud;

        w = glade_xml_get_widget( sxsld->gxml, REMINDER_LIST );
        gtk_clist_freeze( GTK_CLIST(w) );
        gtk_clist_clear( GTK_CLIST(w) );
        add_reminders_to_gui( sxsld->reminderList, sxsld );
        gtk_clist_thaw( GTK_CLIST(w) );
        gnome_druid_set_buttons_sensitive(
                sxsld->sincelast_druid,
                ( gnc_sxsld_get_appropriate_page( sxsld,
                                                  druid_page,
                                                  BACK )
                  != NULL ),
                TRUE, TRUE );
        /* FIXME: this isn't quite right; see the comment in
         * sxsld_remind_row_toggle */
        gnome_druid_set_show_finish( sxsld->sincelast_druid,
                                     !gnc_sxsld_get_appropriate_page( sxsld,
                                                                      druid_page,
                                                                      FORWARD ) );
}

static
gboolean 
reminders_next( GnomeDruidPage *druid_page,
                gpointer arg1, gpointer ud )
{
        GnomeDruidPage *gdp;
        sxSinceLastData *sxsld = (sxSinceLastData*)ud;

        if ( !processed_valid_reminders_listP( sxsld ) ) {
                return TRUE;
        }
        if ( !(gdp = gnc_sxsld_get_appropriate_page( sxsld,
                                                     druid_page,
                                                     FORWARD )) ) {
                DEBUG( "no valid page to switch to" );
                return TRUE;
        }
        gnome_druid_set_page( sxsld->sincelast_druid, gdp );
        return TRUE;
}

static
gboolean 
reminders_back( GnomeDruidPage *druid_page,
                gpointer arg1, gpointer ud )
{
        GnomeDruidPage *gdp;
        sxSinceLastData *sxsld = (sxSinceLastData*)ud;

        if ( !processed_valid_reminders_listP( sxsld ) ) {
                return TRUE;
        }
        if ( !(gdp = gnc_sxsld_get_appropriate_page( sxsld, 
                                                     druid_page,
                                                     BACK )) ) {
                DEBUG( "no valid page to switch to" );
                return TRUE;
        }
        gnome_druid_set_page( sxsld->sincelast_druid, gdp );
        return TRUE;
}

static
gboolean
created_back( GnomeDruidPage *druid_page,
              gpointer arg1, gpointer ud )
{
        sxSinceLastData *sxsld;

        sxsld = (sxSinceLastData*)ud;
        gnc_split_register_save(
                gnc_ledger_display_get_split_register(sxsld->created_ledger),
                TRUE );
        return gen_back( druid_page, arg1, ud );
}

static
gboolean
created_next( GnomeDruidPage *druid_page,
              gpointer arg1, gpointer ud )
{
        sxSinceLastData *sxsld;

        sxsld = (sxSinceLastData*)ud;
        gnc_split_register_save(
                gnc_ledger_display_get_split_register(sxsld->created_ledger),
                TRUE );
        return gen_next( druid_page, arg1, ud );
}

static
void
created_prep( GnomeDruidPage *druid_page,
               gpointer arg1, gpointer ud )
{
        GList *tctList, *tciList, *guidList;
        toCreateTuple *tct;
        toCreateInstance *tci;
        Query *bookQuery, *guidQuery, *q;
        sxSinceLastData *sxsld = (sxSinceLastData*)ud;

        bookQuery = xaccMallocQuery();
        guidQuery = xaccMallocQuery();
        xaccQuerySetBook( bookQuery, gnc_get_current_book() );
        /* Create the appropriate query for the Created ledger; go through
         * the to-create list's instances and add all the created Txn
         * GUIDs. */
        for ( tctList = sxsld->toCreateList;
              tctList;
              tctList = tctList->next ) {
                tct = (toCreateTuple*)tctList->data;
                for ( tciList = tct->instanceList;
                      tciList;
                      tciList = tciList->next ) {
                        tci = (toCreateInstance*)tciList->data;
                        for ( guidList = tci->createdTxnGUIDs;
                              guidList;
                              guidList = guidList->next ) {
                                xaccQueryAddGUIDMatch( guidQuery,
                                                       (GUID*)guidList->data,
                                                       GNC_ID_TRANS,
                                                       QUERY_OR );
                        }
                }
        }
        q = xaccQueryMerge( bookQuery, guidQuery, QUERY_AND );
        gnc_suspend_gui_refresh();
        gnc_ledger_display_set_query( sxsld->created_ledger, q );
        gnc_ledger_display_refresh( sxsld->created_ledger );
        gnc_resume_gui_refresh();
        xaccFreeQuery( q );
        xaccFreeQuery( bookQuery );
        xaccFreeQuery( guidQuery );

        gnome_druid_set_buttons_sensitive(
                sxsld->sincelast_druid,
                ( gnc_sxsld_get_appropriate_page( sxsld,
                                                  druid_page,
                                                  BACK )
                  != NULL ),
                TRUE, TRUE );

        if ( !gnc_sxsld_get_appropriate_page( sxsld,
                                              druid_page,
                                              FORWARD ) ) {
                gnome_druid_set_show_finish( sxsld->sincelast_druid, TRUE );
        }
}

static void
obsolete_prep( GnomeDruidPage *druid_page,
               gpointer arg1, gpointer ud )
{
        sxSinceLastData *sxsld = (sxSinceLastData*)ud;
        add_dead_list_to_gui( sxsld->toRemoveList, sxsld );

        gnome_druid_set_buttons_sensitive(
                sxsld->sincelast_druid,
                ( gnc_sxsld_get_appropriate_page( sxsld,
                                                  druid_page,
                                                  BACK )
                  != NULL ),
                TRUE, TRUE );

        /* This is always the last/finish page. */
        gnome_druid_set_show_finish( sxsld->sincelast_druid, TRUE );
}

static
gboolean
auto_create_back( GnomeDruidPage *druid_page,
                  gpointer arg1, gpointer ud )
{
        sxSinceLastData *sxsld;

        sxsld = (sxSinceLastData*)ud;
        gnc_split_register_save(
                gnc_ledger_display_get_split_register(sxsld->ac_ledger),
                TRUE );
        return gen_back( druid_page, arg1, ud );
}

static
gboolean
auto_create_next( GnomeDruidPage *druid_page,
                  gpointer arg1, gpointer ud )
{
        sxSinceLastData *sxsld;

        sxsld = (sxSinceLastData*)ud;
        gnc_split_register_save(
                gnc_ledger_display_get_split_register(sxsld->ac_ledger),
                TRUE );
        return gen_next( druid_page, arg1, ud );
}

static
void
auto_create_prep( GnomeDruidPage *druid_page,
                  gpointer arg1, gpointer ud )
{
        GList *tctList, *tciList, *guidList;
        toCreateTuple *tct;
        toCreateInstance *tci;
        Query *bookQuery, *guidQuery, *q;
        sxSinceLastData *sxsld = (sxSinceLastData*)ud;

        bookQuery = xaccMallocQuery();
        guidQuery = xaccMallocQuery();
        xaccQuerySetBook( bookQuery, gnc_get_current_book() );
        /* Create the appropriate query for the auto-create-notify ledger; go
         * through the auto-create list's instances and add all the created
         * Txn GUIDs. */
        for ( tctList = sxsld->autoCreateList;
              tctList;
              tctList = tctList->next ) {
                gboolean unused, notifyState;

                tct = (toCreateTuple*)tctList->data;
                xaccSchedXactionGetAutoCreate( tct->sx, &unused, &notifyState );
                if ( !notifyState ) {
                        continue;
                }

                for ( tciList = tct->instanceList;
                      tciList;
                      tciList = tciList->next ) {
                        tci = (toCreateInstance*)tciList->data;
                        for ( guidList = tci->createdTxnGUIDs;
                              guidList;
                              guidList = guidList->next ) {
                                xaccQueryAddGUIDMatch( guidQuery,
                                                       (GUID*)guidList->data,
                                                       GNC_ID_TRANS,
                                                       QUERY_OR );
                        }
                }
        }
        q = xaccQueryMerge( bookQuery, guidQuery, QUERY_AND );
        gnc_suspend_gui_refresh();
        gnc_ledger_display_set_query( sxsld->ac_ledger, q );
        gnc_ledger_display_refresh( sxsld->ac_ledger );
        gnc_resume_gui_refresh();
        xaccFreeQuery( q );
        xaccFreeQuery( bookQuery );
        xaccFreeQuery( guidQuery );

        gnome_druid_set_buttons_sensitive(
                sxsld->sincelast_druid,
                ( gnc_sxsld_get_appropriate_page( sxsld,
                                                  druid_page,
                                                  BACK )
                  != NULL ),
                TRUE, TRUE );

        if ( !gnc_sxsld_get_appropriate_page( sxsld,
                                              druid_page,
                                              FORWARD ) ) {
                gnome_druid_set_show_finish( sxsld->sincelast_druid, TRUE );
        }
}

static
void
to_create_prep( GnomeDruidPage *druid_page,
                gpointer arg1, gpointer ud )
{
        GtkWidget *w;
        sxSinceLastData *sxsld = (sxSinceLastData*)ud;

        w = glade_xml_get_widget( sxsld->gxml, TO_CREATE_LIST );
        gtk_clist_freeze( GTK_CLIST(w) );
        gtk_clist_clear( GTK_CLIST(w) );
        clean_variable_table( sxsld );
        add_to_create_list_to_gui( sxsld->toCreateList, sxsld );
        gtk_clist_thaw( GTK_CLIST(w) );

        
        
        gnome_druid_set_buttons_sensitive(
                sxsld->sincelast_druid,
                ( gnc_sxsld_get_appropriate_page( sxsld,
                                                  druid_page,
                                                  BACK )
                  != NULL ),
                TRUE, TRUE );
        /* Setup next/finish button based on the number of ready-to-go
         * to-create transactions */
        gnome_druid_set_show_finish(
                sxsld->sincelast_druid,
                ( (sxsld_get_future_created_txn_count(sxsld)
                   - sxsld->autoCreatedCount) == 0 ) );
}

static
void
sxsld_revert_to_create_txns( sxSinceLastData *sxsld,
                             toCreateInstance *tci )
{
        GList *l = NULL;
        
        gnc_suspend_gui_refresh();
        for ( l = tci->createdTxnGUIDs;
              l; l = l->next ) {
                Transaction *t;
                t = xaccTransLookup( (GUID*)l->data,
                                     gnc_get_current_book() );
                g_assert( t );
                xaccTransBeginEdit( t );
                xaccTransDestroy( t );
                xaccTransCommitEdit( t );

                /* Remove from master list, too. */
                sxsld->createdTxnGUIDList =
                        g_list_remove(
                                sxsld->createdTxnGUIDList,
                                l->data );
        }
        g_list_free( tci->createdTxnGUIDs );
        tci->createdTxnGUIDs = NULL;
	gnc_resume_gui_refresh();
}

/**
 * @return The count of created transactions.
 **/
static
gint
sxsld_create_to_create_txns( sxSinceLastData *sxsld,
                             toCreateInstance *tci )
{
        gint toRet = 0;
        GList *l = NULL;
        GList *created = NULL;

        /* Don't process instances we've already created transactions for
         * [list_length > 0], which haven't otherwise changed [!dirty]. */
        if ( g_list_length( tci->createdTxnGUIDs ) != 0 ) {
                /* If we've created it and the variables
                 * haven't changed, skip it. */
                if ( ! tci->dirty ) {
                        return toRet;
                }
                /* Otherwise, destroy the transactions and
                 * re-create them below. */

                /* FIXME: this would be better if we could
                 * re-used the existing txns we've already
                 * gone through the pain of creating. */
                sxsld_revert_to_create_txns( sxsld, tci );
        }

        create_transactions_on( tci->parentTCT->sx,
                                tci->date,
                                tci,
                                &created );
        tci->dirty = FALSE;

        /* Add to the Query for that register. */
        for ( l = created; l; l = l->next ) {
                tci->createdTxnGUIDs =
                        g_list_append( tci->createdTxnGUIDs,
                                       (GUID*)l->data );
                toRet++;
        }
        sxsld->createdTxnGUIDList =
                g_list_concat( sxsld->createdTxnGUIDList, created );
        return toRet;
}

/**
 * Do the correct thing for the given toCreateInstance, taking into account
 * what we've done to it before [tci->prevState].  That is: if we previously
 * processed the instance as to-create/as-scheduled, and now we're postponing
 * it, we should remove the previously-created transactions and now add the
 * instance to the postponed list.  See the code for full details on the
 * policy here.
 *
 * @return The count of created transactions.
 **/
static
gint
sxsld_process_to_create_instance( sxSinceLastData *sxsld,
                                  toCreateInstance *tci )
{
        gint toRet = 0;

        /* Undo the previous work. */
        switch ( tci->prevState ) {
        case IGNORE:
                switch ( tci->state ) {
                case IGNORE:
                        /* Keep ignoring. */
                        break;
                case POSTPONE:
                        /* remove from postponed list. */
                        gnc_sx_remove_defer_instance( tci->parentTCT->sx,
                                                      tci->sxStateData );
                        break;
                case TO_CREATE:
                        /* del prev txns. */
                        sxsld_revert_to_create_txns( sxsld, tci );
                        break;
                default:
                        g_assert( FALSE );
                }
                break;
        case POSTPONE:
                if ( tci->state != POSTPONE ) {
                        /* remove from postponed list. */
                        gnc_sx_remove_defer_instance( tci->parentTCT->sx,
                                                      tci->sxStateData );
                }
                break;
        case TO_CREATE:
                if ( tci->state != TO_CREATE ) {
                        /* del prev txns. */
                        sxsld_revert_to_create_txns( sxsld, tci );
                }
                break;
        case UNDEF:
                /* Fine; do nothing. */
                break;
        default:
                g_assert( FALSE );
                break;
        }

        /* Now, process the currently-requested state. */
        switch ( tci->state ) {
        case IGNORE:
                /* Fine ... just ignore it. */
                break;
        case POSTPONE:
                if ( tci->prevState == POSTPONE ) {
                        break;
                }
                /* add to the postponed list. */
                { 
                        char tmpBuf[ MAX_DATE_LENGTH+1 ];
                        printGDate( tmpBuf, tci->date );
                        DEBUG( "Adding defer instance on %s for %s",
                               tmpBuf,
                               xaccSchedXactionGetName( tci->parentTCT->sx ) );
                }
                gnc_sx_add_defer_instance( tci->parentTCT->sx, tci->sxStateData );
                break;
        case TO_CREATE:
                /* Go ahead and create... */
                toRet = sxsld_create_to_create_txns( sxsld, tci );
                break;
        default:
                g_assert( FALSE );
                break;
        }

        tci->prevState = tci->state;

        /* Increment the SX state regardless of what happens above.  The last
         * generated SX instance is the new final state of the SX in all
         * cases [ignored, postponed or created]. */
        {
                gint tmp;
                GDate *last_occur;
                SchedXaction *sx;

                sx = tci->parentTCT->sx;

                /* Only set the last-occur-date, instance count and remaining
                 * occurances if this instance is later than presently-last
                 * definition in the SX; no matter what happens in the SX
                 * dialog, the last instance processed sets the last-occur
                 * date [and other params] to its instance date [and other
                 * params]. */
                last_occur = xaccSchedXactionGetLastOccurDate( sx );
                /* If we don't have anything to do, then just return. */
                if ( g_date_valid( last_occur )
                     && g_date_compare( last_occur, tci->date ) > 0 ) {
                        return toRet;
                }
                xaccSchedXactionSetLastOccurDate( sx, tci->date );

                /* Handle an interesting corner case of postponing or
                 * ignoring the first instance. We only want to incrment the
                 * counters for newly-discovered-as-to-be-created SXes.
                 */
                if ( tci->origState == UNDEF ) {
                        tmp = gnc_sx_get_instance_count( sx, NULL );
                        gnc_sx_set_instance_count( sx, tmp+1 );
                        if ( xaccSchedXactionHasOccurDef( sx ) ) {
                                tmp = xaccSchedXactionGetRemOccur(sx);
                                xaccSchedXactionSetRemOccur( sx, tmp-1 );
                        }
                }
        }

        return toRet;
}

static
gboolean
sxsld_process_to_create_page( sxSinceLastData *sxsld )
{
        GtkCTree *ct;
        GList *tcList, *tcInstList;
        gboolean allVarsBound;
        toCreateTuple *tct;
        toCreateInstance *tci;

        ct = GTK_CTREE( glade_xml_get_widget( sxsld->gxml, TO_CREATE_LIST ) );

        /* First: check to make sure all TCTs are 'ready' [and return if not].
         * Second: create the entries based on the variable bindings. */
        tcList = sxsld->toCreateList;
        if ( tcList == NULL ) {
                DEBUG( "No transactions to create..." );
                return FALSE;
        }

        for ( ; tcList ; tcList = tcList->next ) {
                tct = (toCreateTuple*)tcList->data;
                for ( tcInstList = tct->instanceList;
                      tcInstList;
                      tcInstList = tcInstList->next ) {
                        tci = (toCreateInstance*)tcInstList->data;

                        if ( tci->state == IGNORE
                             || tci->state == POSTPONE ) {
                                continue;
                        }

                        allVarsBound = TRUE;
                        g_hash_table_foreach( tci->varBindings,
                                              andequal_numerics_set,
                                              &allVarsBound );
                        if ( !allVarsBound ) {
                                char tmpBuf[ MAX_DATE_LENGTH+1 ];
                                printGDate( tmpBuf, tci->date );
                                /* FIXME: this should be better-presented to the user. */
                                DEBUG( "SX %s on date %s still has unbound variables.",
                                       xaccSchedXactionGetName(tci->parentTCT->sx), tmpBuf );
                                gtk_ctree_select( ct, tci->node );
                                return TRUE;
                        }
                }
        }

        /* At this point we can assume there are to-create transactions and
         * either the instances are being postponed/ignored, or all variables
         * are bound. */

        tcList = sxsld->toCreateList;
        g_assert( tcList != NULL );

        gnc_suspend_gui_refresh();
        for ( ; tcList ; tcList = tcList->next ) {
                tct = (toCreateTuple*)tcList->data;

                for ( tcInstList = tct->instanceList;
                      tcInstList;
                      tcInstList = tcInstList->next ) {

                        tci = (toCreateInstance*)tcInstList->data;
                        sxsld_process_to_create_instance( sxsld, tci );
                }
        }
        gnc_resume_gui_refresh();
        return FALSE;
}

static
gboolean
to_create_next( GnomeDruidPage *druid_page,
                gpointer arg1, gpointer ud )
{
        sxSinceLastData *sxsld;
        GnomeDruidPage *nextPg;

        sxsld = (sxSinceLastData*)ud;

        /* Do the actual work processing the page. */
        if ( sxsld_process_to_create_page( sxsld ) ) {
                return TRUE;
        }

        /* Figure out the next page, now, given the changes we've made above.
         * This will get us a fix for Bug#95734. */
        nextPg = gnc_sxsld_get_appropriate_page( sxsld,
                                                 GNOME_DRUID_PAGE( druid_page ),
                                                 FORWARD );
        /* We've made the "adjust buttons on disposition-change" fix
         * which will make this assertion true. */
        g_assert( nextPg != NULL );
        gnome_druid_set_page( sxsld->sincelast_druid, nextPg );

        return TRUE;
}

static void
gnc_sxsld_finish( GnomeDruidPage *druid_page,
                  gpointer arg1, gpointer ud )
{
        sxSinceLastData *sxsld = (sxSinceLastData*)ud;
        GList *sxList, *toDelPtr, *elt;
        GtkCList *cl;
        gint row;
        toDeleteTuple *tdt;

        gtk_widget_hide( sxsld->sincelast_window );

        gnc_sxsld_commit_ledgers( sxsld );

        /* If we're finishing from the to-create page, then process the page
         * contents. */
        if ( druid_page ==
             GNOME_DRUID_PAGE( glade_xml_get_widget( sxsld->gxml,
                                                     TO_CREATE_PG ) ) ) {
                DEBUG( "Stopped on to-create-pg" );
                sxsld_process_to_create_page( sxsld );
        }

        /* Deal with the selected obsolete list elts. */
        cl = GTK_CLIST( glade_xml_get_widget( sxsld->gxml,
                                              SX_OBSOLETE_CLIST ) );

        if ( g_list_length( cl->selection ) > 0 ) {
                SchedXactionDialog *sxd;
                sxList = gnc_book_get_schedxactions( gnc_get_current_book() );

                gnc_suspend_gui_refresh();
                for ( toDelPtr = cl->selection;
                      toDelPtr;
                      toDelPtr = toDelPtr->next ) {

                        row = (gint)toDelPtr->data;
                        tdt = (toDeleteTuple*)gtk_clist_get_row_data( cl, row );
                        elt = g_list_find( sxList, tdt->sx );
                        sxList = g_list_remove_link( sxList, elt );

                        xaccSchedXactionFree( (SchedXaction*)elt->data );
                }
                gnc_resume_gui_refresh();

                gnc_book_set_schedxactions( gnc_get_current_book(), sxList );

                sxd = (SchedXactionDialog*)
                        gnc_find_first_gui_component(
                                DIALOG_SCHEDXACTION_CM_CLASS, NULL, NULL );
                if ( sxd ) {
                        gnc_sxd_list_refresh( sxd );
                }
        }

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
        gnc_sx_revert_to_temporal_state( sx, (void*)value );
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

        /* FIXME: This may now be a bug, as we might have changed the SX
         * states. */
        if ( g_list_length( sxsld->createdTxnGUIDList ) == 0 ) {
                /* There's nothing to cancel, so just do so... */
                return FALSE;
        }

        if ( !gnc_verify_dialog( sxsld->sincelast_window, TRUE,
				 lastrun_cancel_check_msg ) ) {
                return TRUE;
        }

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
         * SXes reset [we use the temporal-state-data to take care of this]
         *   . end_date || num_remain_instances
         *   . last_occur_date
         */

        gnc_suspend_gui_refresh();

        /* destroy created transactions */
        if ( g_list_length( sxsld->createdTxnGUIDList ) > 0 ) {
                Transaction *t = NULL;
                for ( l = sxsld->createdTxnGUIDList; l; l = l->next ) {
                        t = xaccTransLookup( (GUID*)l->data,
                                             gnc_get_current_book() );
                        /* we used to assert, but since we allow the user a
                         * register, they may have deleted 't' from their
                         * view.  Thus, if we can't find it, don't die; fixes
                         * Bug#103182. */
                        if ( t != NULL )
                        {
                          xaccTransBeginEdit( t );
                          xaccTransDestroy( t );
                          xaccTransCommitEdit( t );
                          t = NULL;
                        }
                }
        }

        /* Remove postponed SXes from their postponed lists, unless they were
         * originally postponed. */
        {
                GList *tcList, *tciList;
                toCreateTuple *tct;
                toCreateInstance *tci;

                for ( tcList = sxsld->toCreateList;
                      tcList;
                      tcList = tcList->next ) {
                        tct = (toCreateTuple*)tcList->data;
                        for ( tciList = tct->instanceList;
                              tciList;
                              tciList = tciList->next ) {
                                tci = (toCreateInstance*)tciList->data;
                                if ( tci->prevState == POSTPONE
                                     && tci->origState    != POSTPONE ) {
                                        /* Any valid [non-null] 'prevState !=
                                         * POSTPONE' sx temporal state
                                         * pointers will be destroyed at the
                                         * destruction of the dialog [the
                                         * non-cancel case], so if we need to
                                         * deal with those here, we should do
                                         * so.
                                         */
                                        gnc_sx_remove_defer_instance( tct->sx, tci->sxStateData );
                                        gnc_sx_destroy_temporal_state( tci->sxStateData );
                                        tci->sxStateData = NULL;
                                }
                        }
                }
        }

        /* Restore the temporal state of all SXes. 
         * This is in sxInitStates [a bunch of opaque void *'s ... which
         * should be freed when we're done to prevent a memory leak.] */
        g_hash_table_foreach( sxsld->sxInitStates,
                              restore_sx_temporal_state,
                              (gpointer)sxsld );
        /* This will get destroyed when the dialog is, which will happen
         * shortly after this return. */

        gnc_resume_gui_refresh();
        return FALSE;
}


static void
sxsincelast_init( sxSinceLastData *sxsld )
{
        GtkWidget *w;
        GtkObject *o;
        GnomeDruidPage *nextPage;
        int i;
        static widgetSignalHandlerTuple widgets[] = {
                { SINCELAST_DRUID, "cancel",  sxsincelast_druid_cancelled },

                { REMINDER_LIST, "tree-select-row",   sxsld_remind_row_toggle },
                { REMINDER_LIST, "tree-unselect-row", sxsld_remind_row_toggle },
                
                { TO_CREATE_LIST, "tree-select-row",   sxsincelast_tc_row_sel },
                { TO_CREATE_LIST, "tree-unselect-row", sxsincelast_tc_row_unsel },

                { SX_OBSOLETE_CLIST, "select-row",   sxsld_obsolete_row_toggle },
                { SX_OBSOLETE_CLIST, "unselect-row", sxsld_obsolete_row_toggle },

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
                  reminders_prep, reminders_back, reminders_next,
                  gnc_sxsld_finish, cancel_check },

                { AUTO_CREATE_NOTIFY_PG,
                  auto_create_prep, auto_create_back, auto_create_next,
                  gnc_sxsld_finish, cancel_check },

                { TO_CREATE_PG,
                  to_create_prep, gen_back, to_create_next,
                  gnc_sxsld_finish, cancel_check },

                { CREATED_PG,
                  created_prep, created_back, created_next,
                  gnc_sxsld_finish, cancel_check },

                { OBSOLETE_PG,
                  obsolete_prep, gen_back, gen_next,
                  gnc_sxsld_finish, cancel_check },

                { NULL, NULL, NULL, NULL, NULL, NULL }
        };

        static const struct optionMenuTuple {
                char *name;
                void (*fn)();
        } optionMenus[] = {
                { SX_DISPOSITION_OPT, sxsld_disposition_changed },
                { NULL, NULL }
        };


        gnc_register_gui_component( DIALOG_SXSINCELAST_CM_CLASS,
                                    NULL,
                                    sxsincelast_close_handler,
                                    sxsld->sincelast_window );

        gtk_signal_connect( GTK_OBJECT(sxsld->sincelast_window), "destroy",
                            GTK_SIGNAL_FUNC( sxsincelast_destroy ), sxsld );

	dialog_widgets_attach_handlers(sxsld->gxml, widgets, sxsld);
        druid_pages_attach_handlers( sxsld->gxml, pages, sxsld );

        /* gnc-init the option menu[s]. */
        for ( i=0; optionMenus[i].name != NULL; i++ ) {
                w = glade_xml_get_widget( sxsld->gxml, optionMenus[i].name );
                gnc_option_menu_init( w );
                o = GTK_OBJECT(gtk_option_menu_get_menu(GTK_OPTION_MENU(w)));
                gtk_signal_connect( o, "selection-done",
                                    GTK_SIGNAL_FUNC( optionMenus[i].fn ),
                                    sxsld );
        }

        /* set all to-create clist columns to auto-resize. */
        w = glade_xml_get_widget( sxsld->gxml, TO_CREATE_LIST );
        clist_set_all_cols_autoresize(GTK_CLIST(w), TO_CREATE_LIST_WIDTH);
        w = glade_xml_get_widget( sxsld->gxml, REMINDER_LIST );
        clist_set_all_cols_autoresize(GTK_CLIST(w), REMINDER_LIST_WIDTH);
        w = glade_xml_get_widget( sxsld->gxml, SX_OBSOLETE_CLIST );
        clist_set_all_cols_autoresize(GTK_CLIST(w), SX_OBSOLETE_CLIST_WIDTH);

        sxsld->prog = GTK_PROGRESS_BAR(glade_xml_get_widget( sxsld->gxml,
                                                             WHAT_TO_DO_PROGRESS ));

        sxsld->toCreateStatus =
                GTK_STATUSBAR(
                        glade_xml_get_widget( sxsld->gxml, TO_CREATE_STATUS ) );
        sxsld->statusCtxId =
                gtk_statusbar_get_context_id( sxsld->toCreateStatus,
                                              /* Sure, we're overusing this
                                               * string, but I don't see why
                                               * the Statusbar even
                                               * cares... */
                                              TO_CREATE_STATUS );

        create_autoCreate_ledger( sxsld );
        create_created_ledger( sxsld );
        create_to_create_ledger( sxsld );

        {
                int width, height;
                gnc_get_window_size( SXSLD_WIN_PREFIX, &width, &height );
                if ( width != 0 && height != 0 ) {
                        gtk_window_set_default_size( GTK_WINDOW(sxsld->sincelast_window),
                                                     width, height );
                }
        }

        gtk_widget_show_all( sxsld->sincelast_window );

        process_auto_create_list( sxsld->autoCreateList, sxsld );

        w = glade_xml_get_widget( sxsld->gxml, WHAT_TO_DO_PG );
        nextPage = gnc_sxsld_get_appropriate_page( sxsld,
                                                   GNOME_DRUID_PAGE(w),
                                                   FORWARD );

        /* If there's nowhere to go, then we shouldn't have been started at
         * all [ie., ..._populate should have returned FALSE]. */
        g_assert( nextPage );

        gnome_druid_set_page( sxsld->sincelast_druid, nextPage );
}

static
void
sxsincelast_save_size( sxSinceLastData *sxsld )
{
        gint x, y, w, h, d;
        gdk_window_get_geometry( sxsld->sincelast_window->window,
                                 &x, &y, &w, &h, &d );
        gnc_save_window_size( SXSLD_WIN_PREFIX, w, h );
}

static void
generate_instances( SchedXaction *sx,
                    GDate *end,
                    GDate *reminderEnd,
                    GList **instanceList,
                    GList **reminderList,
                    GList **deadList )
{
        GDate gd;
        toCreateInstance *tci;
        reminderTuple *rt;
        reminderInstanceTuple *rit;
        void *seqStateData;

        g_assert( g_date_valid(end) );
        g_assert( g_date_valid(reminderEnd) );

        g_date_clear( &gd, 1 );

        /* Process valid next instances. */
        seqStateData = gnc_sx_create_temporal_state( sx );
        //gd = xaccSchedXactionGetNextInstance( sx, seqStateData );
        gd = xaccSchedXactionGetInstanceAfter( sx, &gd, seqStateData );
        while ( g_date_valid(&gd)
                && g_date_compare( &gd, end ) <= 0 ) {

                tci = g_new0( toCreateInstance, 1 );

                tci->dirty     = FALSE;
                tci->date      = g_date_new();
                *tci->date     = gd;
                tci->origState = UNDEF;
                tci->state     = TO_CREATE;
                tci->prevState = UNDEF;
                tci->sxStateData =
                        gnc_sx_clone_temporal_state( seqStateData );
                *instanceList  = g_list_append( *instanceList, tci );

                gnc_sx_incr_temporal_state( sx, seqStateData );
                gd = xaccSchedXactionGetInstanceAfter( sx, &gd, seqStateData );
        }

        /* Process reminder instances or add to dead list [if we have one] */
        if ( g_date_valid( &gd ) ) {
                rt = g_new0( reminderTuple, 1 );
                rt->sx = sx;
                rt->instanceList = NULL;
                while ( g_date_valid(&gd)
                        && g_date_compare( &gd, reminderEnd ) <= 0 ) {

                        rit = g_new0( reminderInstanceTuple, 1 );
                        rit->endDate     = g_date_new();
                        *rit->endDate    = *end;
                        rit->occurDate   = g_date_new();
                        *rit->occurDate  = gd;
                        rit->isSelected  = FALSE;
                        rit->parentRT    = rt;
                        rit->sxStateData =
                                gnc_sx_clone_temporal_state( seqStateData );
                        rt->instanceList = g_list_append( rt->instanceList, rit );

                        gnc_sx_incr_temporal_state( sx, seqStateData );
                        gd = xaccSchedXactionGetInstanceAfter( sx, &gd, seqStateData );
                }
                if ( rt->instanceList != NULL ) {
                        *reminderList = g_list_append( *reminderList, rt );
                } else {
                        g_free( rt );
                }
                rt = NULL;
        } else if ( deadList ) {
                toDeleteTuple *tdt;

                tdt = g_new0( toDeleteTuple, 1 );
                tdt->sx = sx;
                tdt->endDate = g_date_new();
                *tdt->endDate = gd;
                *deadList = g_list_append( *deadList, tdt );
        } /* else { this else intentionally left blank: drop the SX on the
           * floor at this point. } */

        gnc_sx_destroy_temporal_state( seqStateData );
        seqStateData = NULL;
}

static void
_free_varBindings_hash_elts( gpointer key, gpointer value, gpointer data )
{
        g_assert( key );
        g_free( key );
        if ( value ) 
                g_free( value );
}

static void
process_auto_create_list( GList *autoCreateList, sxSinceLastData *sxsld )
{
        GList *l;
        toCreateTuple *tct;
        toCreateInstance *tci;
        GList *instances;
        int count, total;

        count = 0;
        total = 0;
        /* get an accurate count of how many SX instances we're going to
         * create. */
        for ( l = autoCreateList; l; l = l->next ) {
                total += g_list_length( ((toCreateTuple*)l->data)->instanceList );
        }
        gtk_progress_configure( GTK_PROGRESS(sxsld->prog), 0, 0, total );
        gnc_suspend_gui_refresh();

        for ( ; autoCreateList ; autoCreateList = autoCreateList->next ) {
                tct = (toCreateTuple*)autoCreateList->data;
                
                for ( instances = tct->instanceList;
                      instances;
                      instances = instances->next ) {
                        tci = (toCreateInstance*)instances->data;
                        sxsld->autoCreatedCount +=
                                sxsld_process_to_create_instance( sxsld, tci );
                }
        }
        gnc_resume_gui_refresh();
}

static
void
add_to_create_list_to_gui( GList *toCreateList, sxSinceLastData *sxsld )
{
        toCreateTuple *tct;
        toCreateInstance *tci;
        GtkCTree *ct;
        GtkCTreeNode *sxNode;
        GtkCTreeNode *firstToBeProcessedRow;
        char *rowText[ TO_CREATE_LIST_WIDTH ];
        GList *insts;

        ct = GTK_CTREE( glade_xml_get_widget( sxsld->gxml, TO_CREATE_LIST ) );

        firstToBeProcessedRow = NULL;
        for ( ; toCreateList ; toCreateList = toCreateList->next ) {
                tct = (toCreateTuple*)toCreateList->data;

                rowText[0] = xaccSchedXactionGetName( tct->sx );
                rowText[1] = "";

                sxNode = gtk_ctree_insert_node( ct, NULL, NULL,
                                                rowText,
                                                0, NULL, NULL, NULL, NULL,
                                                FALSE, TRUE );

                for ( insts = tct->instanceList;
                      insts;
                      insts = insts->next ) {
                        gboolean allVarsBound;

                        tci = (toCreateInstance*)insts->data;
                
                        /* tct->{sx,date} are already filled in. */
                        if ( ! tci->varBindings ) {
                                tci->varBindings = g_hash_table_new( g_str_hash,
                                                                     g_str_equal );

                                sxsl_get_sx_vars( tci->parentTCT->sx,
                                                  tci->varBindings );
                        }

                        rowText[0] = g_new0( char, MAX_DATE_LENGTH+1 );
                        printGDate( rowText[0], tci->date );
                        

                        switch ( tci->state ) {
                        case TO_CREATE:
                                allVarsBound = TRUE;
                                g_hash_table_foreach( tci->varBindings,
                                                      andequal_numerics_set,
                                                      &allVarsBound );
                                rowText[1] = ( allVarsBound
                                               ? _( "Ready to create" )
					       /* READY_TEXT */ 
					       : _( "Needs values for variables" )
                                               /* NEEDS_BINDINGS_TEXT */ );
                                break;
                        case IGNORE:
			    rowText[1] = _( "Ignored" ) /* IGNORE_TEXT */ ;
			    break;
                        case POSTPONE:
			    rowText[1] = _( "Postponed" ) /* POSTPONE_TEXT */ ;
			    break;
                        default:
                                g_assert( FALSE );
                        }
                                
                        tci->node = gtk_ctree_insert_node( ct, sxNode, NULL,
                                                           rowText,
                                                           0, NULL, NULL, NULL, NULL,
                                                           TRUE, FALSE );
                        if ( !allVarsBound && !firstToBeProcessedRow ) {
                                firstToBeProcessedRow = tci->node;
                        }
                        gtk_ctree_node_set_row_data( ct, tci->node, tci );
                        g_free( rowText[0] );
                }
        }

        /* Setup the first thing to be processed, or disable controls. */
        if ( firstToBeProcessedRow ) {
                gtk_ctree_select( ct, firstToBeProcessedRow );
                sxsld_set_sensitive_tci_controls( sxsld, TRUE );
        } else {
                sxsld_set_sensitive_tci_controls( sxsld, FALSE );
        }
}

static
void
add_reminders_to_gui( GList *reminderList, sxSinceLastData *sxsld )
{
        GtkCTree *ctree;
        GtkCTreeNode *sxNode, *instNode;
        char *rowText[REMINDER_LIST_WIDTH];
        reminderTuple *rt;
        GList *instances;
        reminderInstanceTuple *rit;
        FreqSpec *fs;
        GString *freqSpecStr;

        ctree = GTK_CTREE( glade_xml_get_widget( sxsld->gxml,
                                                 REMINDER_LIST ) );

        for ( ; reminderList; reminderList = reminderList->next ) {
                rt = (reminderTuple*)reminderList->data;

                rowText[0] = xaccSchedXactionGetName( rt->sx );
                fs = xaccSchedXactionGetFreqSpec( rt->sx );
                freqSpecStr = g_string_sized_new( 16 );
                xaccFreqSpecGetFreqStr( fs, freqSpecStr );
                rowText[1] = freqSpecStr->str;
                rowText[2] = ""; /* Days Away */
                sxNode = gtk_ctree_insert_node( ctree, NULL, NULL, rowText,
                                                0, /* spacing */
                                                NULL, NULL, NULL, NULL, /* pixmaps */
                                                FALSE, /* leafP */
                                                TRUE ); /* expandedP */
                g_string_free( freqSpecStr, TRUE );

                /* The SX node itself isn't selectable; only the
                 * instances. */
                gtk_ctree_node_set_selectable( ctree, sxNode, FALSE );
                for ( instances = rt->instanceList;
                      instances;
                      instances = instances->next ) {
                        rit = (reminderInstanceTuple*)instances->data;

                        rowText[0] = g_new0( gchar, MAX_DATE_LENGTH+1 );
                        printGDate( rowText[0], rit->occurDate );
                        rowText[1] = "";
                        rowText[2] = g_new0( gchar, 5 ); /* FIXME: appropriate size? */
                        sprintf( rowText[2], "%d",
                                 (g_date_julian(rit->occurDate)
                                  - g_date_julian(rit->endDate)) );

                        instNode = gtk_ctree_insert_node( ctree, sxNode, NULL,
                                                          rowText,
                                                          0, NULL, NULL, NULL, NULL,
                                                          TRUE, TRUE );
                        gtk_ctree_node_set_row_data( ctree,
                                                     instNode,
                                                     (gpointer)rit );
                        gtk_signal_handler_block_by_func( GTK_OBJECT(ctree),
                                                          sxsld_remind_row_toggle,
                                                          sxsld ); 
                        if ( rit->isSelected ) {
                                gtk_ctree_select( ctree, instNode );
                        }
                        gtk_signal_handler_unblock_by_func( GTK_OBJECT(ctree),
                                                            sxsld_remind_row_toggle,
                                                            sxsld );
                        g_free( rowText[0] );
                        g_free( rowText[2] );
                }
        }
}

static void
add_dead_list_to_gui(GList *removeList, sxSinceLastData *sxsld)
{
        GtkCList *cl;
        char *rowtext[3];
        int row;
        GString *tmp_str;
        toDeleteTuple *tdt;
        FreqSpec *fs;
        cl = GTK_CLIST( glade_xml_get_widget( sxsld->gxml,
                                              SX_OBSOLETE_CLIST ));

        tmp_str = g_string_new(NULL);
        //rowtext[2] = g_new0(gchar, MAX_DATE_LENGTH+1 ); 

        gtk_clist_freeze( cl );
        gtk_clist_clear( cl );
        gtk_signal_handler_block_by_func( GTK_OBJECT(cl),
                                          sxsld_obsolete_row_toggle,
                                          sxsld );

        for ( row = 0; removeList;
              row++, removeList = removeList->next ) {
                tdt = (toDeleteTuple*)removeList->data;

                rowtext[0] = xaccSchedXactionGetName( tdt->sx );

                fs = xaccSchedXactionGetFreqSpec( tdt->sx );
                xaccFreqSpecGetFreqStr( fs, tmp_str );
                rowtext[1] = tmp_str->str;

                rowtext[2] = g_strdup( _("Obsolete") );

                gtk_clist_insert( cl, row, rowtext );
                gtk_clist_set_row_data( cl, row, tdt );
                if ( tdt->isSelected ) {
                        gtk_clist_select_row( cl, row, 0 );
                }
        }
        gtk_signal_handler_unblock_by_func( GTK_OBJECT(cl),
                                            sxsld_obsolete_row_toggle,
                                            sxsld );
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
        GList **containingList;
        reminderInstanceTuple *rit;
        toCreateTuple *tct;
        toCreateInstance *tci;
        gboolean autoCreateOpt, notifyOpt;

        tct = NULL;
        for ( ; goodList ; goodList = goodList->next ) {
                rit = (reminderInstanceTuple*)goodList->data;

                /* skip over reminders we've already created [in the
                 * past]. */
                if ( rit->resultantTCI )
                        continue;

                xaccSchedXactionGetAutoCreate( rit->parentRT->sx,
                                               &autoCreateOpt, &notifyOpt );
                containingList = ( autoCreateOpt
                                   ? &sxsld->autoCreateList
                                   : &sxsld->toCreateList );
                for ( list = *containingList;
                      list;
                      list = list->next ) {
                        tct = (toCreateTuple*)list->data;
                        /* Find any already-existing toCreateTuples to add to...*/
                        if ( tct->sx == rit->parentRT->sx ) {
                                break;
                        }
                }
                if ( !list ) {
                        tct = g_new0( toCreateTuple, 1 );
                        tct->sx = rit->parentRT->sx;
                        *containingList =
                                g_list_append( *containingList, tct );
                }

                tci = g_new0( toCreateInstance, 1 );
                tci->dirty       = FALSE;
                tci->parentTCT   = tct;
                tci->date        = g_date_new();
                *tci->date       = *rit->occurDate;
                tci->state       = TO_CREATE;
                tci->prevState   = UNDEF;
                tci->origState   = UNDEF;
                tci->varBindings = NULL;
                tci->node        = NULL;
                tci->sxStateData = rit->sxStateData;
                
                tct->instanceList =
                        g_list_append( tct->instanceList, tci );
                
                /* special auto-create-opt processing; process it now. */
                if ( autoCreateOpt ) {
                        list = NULL;
                        list = g_list_append( list, tct );
                        process_auto_create_list( list, sxsld );
                        list = NULL;
                }

                /* save the resultant just-created TCI in the RIT in case
                 * things change later. */
                rit->resultantTCI = tci;
        }
}

/**
 * @see gnc_ui_sxsincelast_dialog_create for the return value definition.
 **/
static
gint
sxsincelast_populate( sxSinceLastData *sxsld )
{
        int toRet = 0;
        gboolean onlyNoNotify = TRUE;
        GList *sxList, *instanceList, *l, **containingList;
        SchedXaction *sx;
        GDate end, endPlusReminders;
        gint daysInAdvance;
        gboolean autocreateState, notifyState;
        toCreateTuple *tct;
        toCreateInstance *tci;

        instanceList = NULL;
        sxList = gnc_book_get_schedxactions( gnc_get_current_book () );

        if ( sxList == NULL ) {
                DEBUG( "No scheduled transactions to populate." );
                return toRet;
        }

        for ( ; sxList; sxList = sxList->next ) {
                sx = (SchedXaction*)sxList->data;
                
                /* Store initial state of SX. */
                if ( g_hash_table_lookup( sxsld->sxInitStates, sx )
                     != NULL ) {
                        PERR( "Why are we able to find a SX initial state "
                              "hash entry for something we're seeing for "
                              "the first time?" );
                        return toRet;
                }
                {
                        void *sx_state;
                        sx_state = gnc_sx_create_temporal_state( sx );
                        g_hash_table_insert( sxsld->sxInitStates,
                                             sx, sx_state );
                        sx_state = NULL;
                }

                g_date_set_time( &end, time(NULL) );
                daysInAdvance = xaccSchedXactionGetAdvanceCreation( sx );
                g_date_add_days( &end, daysInAdvance );
                
                endPlusReminders = end;
                daysInAdvance = xaccSchedXactionGetAdvanceReminder( sx );
                g_date_add_days( &endPlusReminders, daysInAdvance );

                /* Handle postponed instances.
                 *
                 * Postponed instances, by definition, are always at the
                 * front of the instance list.  As well, they're always valid
                 * instances [not reminders]. */

                /* FIXME: postponed instances _may_ create an obsolete
                 * instance. */
                {
                        GList *postponed, *l;

                        postponed = gnc_sx_get_defer_instances( sx );

                        for ( l = postponed; l; l = l->next ) {
                                onlyNoNotify = FALSE;

                                tci = g_new0( toCreateInstance, 1 );
                                tci->sxStateData = (void*)l->data;
                                tci->date        = g_date_new();
                                *tci->date       =
                                        xaccSchedXactionGetNextInstance(
                                                sx, tci->sxStateData );
                                tci->dirty       = FALSE;
                                tci->state       = POSTPONE;
                                tci->prevState   = POSTPONE;
                                tci->origState   = POSTPONE;

                                instanceList = g_list_append( instanceList, tci );
                                tci = NULL;
                        }
                        
                }

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
                /* Figure out the appropriate list to place the new TCT on. */
                containingList = ( autocreateState
                                   ? &sxsld->autoCreateList
                                   : &sxsld->toCreateList );

                tct = g_new0( toCreateTuple, 1 );
                tct->sx = sx;
                for ( l = instanceList ; l; l = l->next ) {

                        /* only count the no-notify txns for this. */
                        if ( autocreateState && !notifyState ) {
                                onlyNoNotify &= (!notifyState);
                                toRet++;
                        }

                        tci = (toCreateInstance*)l->data;
                        tci->parentTCT = tct;
                        
                        tct->instanceList =
                                g_list_append( tct->instanceList, tci );
                }

                g_list_free( instanceList );
                instanceList = NULL;

                /* abstractly place the TCT onto the afore-determined list. */
                *containingList = g_list_append( *containingList, tct );
        }

        /* Return appropriately. */
        {
                gboolean stuffToDo = 
                        ( g_list_length( sxsld->toRemoveList )    > 0
                          || g_list_length( sxsld->reminderList ) > 0
                          || g_list_length( sxsld->toCreateList ) > 0 );
                if ( onlyNoNotify && !stuffToDo ) {
                        toRet = -(toRet);
                }

                if ( toRet == 0
                     && ( stuffToDo
                          || g_list_length( sxsld->autoCreateList ) > 0 ) ) {
                        toRet = INT_MAX;
                }
        }

        /* if we're about to return a negative value [indicating only
         * auto-create no-notify txns], then actually create them. */
        if ( toRet < 0 ) {
                process_auto_create_list( sxsld->autoCreateList, sxsld );
        }

        return toRet;
}

static void
sxsincelast_close_handler( gpointer ud )
{
        sxSinceLastData *sxsld = (sxSinceLastData*)ud;
        
        gtk_widget_hide( sxsld->sincelast_window );
        sxsincelast_save_size( sxsld );
        gtk_widget_destroy( sxsld->sincelast_window );
        /* The data will be cleaned up in the destroy handler. */
}

static void
andequal_numerics_set( gpointer key, gpointer value, gpointer data )
{
        gboolean *allVarsBound = data;
        if ( strcmp( (gchar*)key, "i" ) == 0 ) {
                return;
        }
        *allVarsBound &= (value != NULL);
}

static void
sxsincelast_entry_changed( GtkEditable *e, gpointer ud )
{
        sxSinceLastData *sxsld;
        gchar *varName;
        toCreateInstance *tci;
        gchar *entryText;
        gnc_numeric *num, *ourNum;
        GHashTable *dummyVarHash;
        static const int MSG_BUF_LEN = 127;
        char msgBuf[MSG_BUF_LEN+1];

        sxsld = (sxSinceLastData*)ud;

        tci = (toCreateInstance*)gtk_object_get_data( GTK_OBJECT(e), "tci" );
        g_assert( tci == sxsld->curSelTCI );

        varName = (gchar*)gtk_object_get_data( GTK_OBJECT(e), "varName" );
        num = (gnc_numeric*)gtk_object_get_data( GTK_OBJECT(e), "numeric" );
        entryText = gtk_editable_get_chars( e, 0, -1 );
        dummyVarHash = g_hash_table_new( NULL, NULL );
        /* FIXME?: Should be using xaccParseAmount instead of
         * parser_parse_separate_vars? */
        gtk_statusbar_pop( sxsld->toCreateStatus, sxsld->statusCtxId );

        if ( !gnc_exp_parser_parse_separate_vars( entryText, num,
                                                  NULL, dummyVarHash ) ) {
                num = NULL;
                if ( entryText != NULL
                     && strlen(entryText) > 0 ) {
                        snprintf( msgBuf, MSG_BUF_LEN,
                                  "error parsing entry near \"%s\"", entryText );
                        gtk_statusbar_push( sxsld->toCreateStatus,
                                            sxsld->statusCtxId,
                                            msgBuf );
                }
        } else if ( g_hash_table_size( dummyVarHash ) != 0 ) {
                num = NULL;
                snprintf( msgBuf, MSG_BUF_LEN,
                          "No new variables allowed in "
                          "expression \"%s\"", entryText );
                gtk_statusbar_push( sxsld->toCreateStatus,
                                    sxsld->statusCtxId,
                                    msgBuf );
        } else if ( gnc_numeric_check( *num ) != GNC_ERROR_OK ) {
                snprintf( msgBuf, MSG_BUF_LEN,
                          "Entry \"%s\" is not "
                          "parseable", entryText );
                gtk_statusbar_push( sxsld->toCreateStatus,
                                    sxsld->statusCtxId,
                                    msgBuf );
                num = NULL;
        } else {
                snprintf( msgBuf, MSG_BUF_LEN,
                          "%f", gnc_numeric_to_double( *num ) );
                gtk_statusbar_push( sxsld->toCreateStatus,
                                    sxsld->statusCtxId,
                                    msgBuf );
        }

        g_hash_table_foreach( dummyVarHash,
                              _free_varBindings_hash_elts,
                              NULL );
        g_hash_table_destroy( dummyVarHash );

        {
                gpointer maybeKey, maybeValue;
                
                ourNum = NULL;
                if ( num ) {
                        ourNum = g_new0( gnc_numeric, 1 );
                        *ourNum = *num;
                }
                if ( g_hash_table_lookup_extended( tci->varBindings, varName,
                                                   &maybeKey, &maybeValue ) ) {
                        g_hash_table_remove( tci->varBindings, maybeKey );
                        /* only if not null. */
                        if ( maybeValue ) {
                                g_free( maybeValue );
                        }
                }
                g_hash_table_insert( tci->varBindings, varName, ourNum );
                tci->dirty = TRUE;
        }

        
        {
                GtkCTree *ct;
                gboolean allVarsBound = TRUE;

                /* If there are no un-bound variables, then set the 'ready-to-go'
                   flag to 'y'. */
                g_hash_table_foreach( tci->varBindings,
                                      andequal_numerics_set,
                                      &allVarsBound );
                ct = GTK_CTREE(glade_xml_get_widget( sxsld->gxml, TO_CREATE_LIST ));
                gtk_ctree_node_set_text( ct, tci->node, 1,
                                         ( allVarsBound
                                           ? _( READY_TEXT )
                                           : _( NEEDS_BINDINGS_TEXT ) ) );
        }
}

static void
sxsincelast_destroy( GtkObject *o, gpointer ud )
{
        sxSinceLastData *sxsld = (sxSinceLastData*)ud;

        /* appropriate place to destroy data structures */
        clean_sincelast_data( sxsld );

        gnc_ledger_display_close( sxsld->ac_ledger );
        sxsld->ac_ledger = NULL;

        gnc_ledger_display_close( sxsld->created_ledger );
        sxsld->created_ledger = NULL;

        gnc_ledger_display_close( sxsld->to_create_ledger );
        sxsld->to_create_ledger = NULL;

	gnc_unregister_gui_component_by_data( DIALOG_SXSINCELAST_CM_CLASS,
					      sxsld->sincelast_window );

        g_free( sxsld );
}

/**
 * Used to copy the varBinding GHashTable.
 **/
static
void
gnc_sxsl_copy_ea_hash( gpointer key,
                       gpointer value,
                       gpointer user_data )
{
        gchar *name = (gchar*)key;
        gnc_numeric *val = (gnc_numeric*)value;
        gnc_numeric *newVal;
        GHashTable *table = (GHashTable*)user_data;

        newVal = g_new0( gnc_numeric, 1 );
        *newVal = gnc_numeric_error( -2 );
        if ( val )
                *newVal = *val;

        g_assert( name );

        g_hash_table_insert( table,
                             (gpointer)g_strdup( name ),
                             (gpointer)newVal );
}

static
void
gnc_sxsl_del_vars_table_ea( gpointer key,
                            gpointer value,
                            gpointer user_data )
{
        g_assert( key );
        if ( key )
                g_free( (gchar*)key );
        if ( value )
                g_free( (gnc_numeric*)value );
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
        toCreateInstance *tci;
        gnc_commodity *commonCommodity = NULL;
        GHashTable *actualVars;
        gnc_numeric *varIValue;

        errFlag = FALSE;

        /* FIXME: In general, this should [correctly] deal with errors such
           as not finding the approrpiate Accounts and not being able to
           parse the formula|credit/debit strings. */

        /* FIXME: when we copy the trans_onto_trans, we don't want to copy
           the Split's kvp_frames... */

        createUD = (createData*)d;
        tci = createUD->tci;

        newT = xaccMallocTransaction(gnc_get_current_book ());
        xaccTransBeginEdit( newT );
        /* the action and description/memo are in the template */
        gnc_copy_trans_onto_trans( t, newT, FALSE, FALSE );

        xaccTransSetDate( newT,
                          g_date_day( tci->date ),
                          g_date_month( tci->date ),
                          g_date_year( tci->date ) );

        /* the accounts and amounts are in the kvp_frames of the splits. */
        osList = xaccTransGetSplitList( t );
        sList = xaccTransGetSplitList( newT );
        if ( (osList == NULL) || (sList == NULL) ) {
                PERR( "\tseen transaction w/o splits. :(" );
                xaccTransDestroy( newT );
                xaccTransCommitEdit( newT );
                return FALSE;
        }

        /* Setup the predefined variables for credit/debit formula
         * processing. */
        actualVars = g_hash_table_new( g_str_hash, g_str_equal );
        if ( tci->varBindings != NULL ) {
                g_hash_table_foreach( tci->varBindings,
                                      gnc_sxsl_copy_ea_hash, actualVars );
        }
        varIValue = g_new0( gnc_numeric, 1 );
        *varIValue =
                gnc_numeric_create(
                        gnc_sx_get_instance_count( tci->parentTCT->sx,
                                                   tci->sxStateData ),
                        1 );
        /* It's really important that we strdup "i" here, so we can
         * generically cleanup with a simple 'foreach' that blindly frees the
         * keys, below. */
        g_hash_table_insert( actualVars, g_strdup("i"), varIValue );

        for ( ; sList && osList ;
              sList = sList->next, osList = osList->next ) {
                Account *acct;

                split = (Split*)sList->data;

                /* FIXME: Ick.  This assumes that the split lists will be
                   ordered identically. :( I think it's fair to say they
                   will, but I'd rather not have to count on it. --jsled */
                split_kvpf = xaccSplitGetSlots( (Split*)osList->data );

                /* from-transaction of splits */
                /* This needs to be before the value setting [below] so the
                 * balance calculations can work. */
                {
                        GUID                *acct_guid;
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
#if 0 /* debug */
                        DEBUG( "Got account with name \"%s\"",
                                xaccAccountGetName( acct ) );
#endif /* 0 -- debug */
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
                }

                /* commonCommodity = xaccTransGetCurrency( t ); */
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
                                                                           &parseErrorLoc,
                                                                           actualVars ) ) {
                                        PERR( "Error parsing credit formula \"%s\" at \"%s\": %s",
                                              str, parseErrorLoc, gnc_exp_parser_error_string() );
                                        errFlag = TRUE;
                                        break;
                                }
#if 0 /* debug */
                                DEBUG( "gnc_numeric::credit: \"%s\" -> %s [%s]",
                                       str, gnc_numeric_to_string( credit_num ),
                                       gnc_numeric_to_string( gnc_numeric_reduce( credit_num ) ) );
#endif /* 0 -- debug */
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
                                                                           &parseErrorLoc,
                                                                           actualVars ) ) {
                                        PERR( "Error parsing debit_formula \"%s\" at \"%s\": %s",
                                              str, parseErrorLoc, gnc_exp_parser_error_string() );
                                        errFlag = TRUE;
                                        break;
                                }

#if 0 /* debug */
                                DEBUG( "gnc_numeric::debit: \"%s\" -> %s [%s]",
                                       str, gnc_numeric_to_string( debit_num ),
                                       gnc_numeric_to_string( gnc_numeric_reduce( debit_num ) ) );
#endif /* 0 -- debug */
                        }
                        
                        final = gnc_numeric_sub_fixed( debit_num, credit_num );
                        
                        gncn_error = gnc_numeric_check( final );
                        if ( gncn_error != GNC_ERROR_OK ) {
                                PERR( "Error %d in final gnc_numeric value", gncn_error );
                                errFlag = TRUE;
                                break;
                        }
#if 0 /* debug */
                        DEBUG( "gnc_numeric::final: \"%s\"",
                               gnc_numeric_to_string( final ) );
#endif /* 0 -- debug */

                        xaccSplitSetValue( split, final );
                        xaccSplitScrub( split );
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

                xaccAccountCommitEdit( acct );
        }


        /* Cleanup actualVars table. */
        {
                g_hash_table_foreach( actualVars,
                                      gnc_sxsl_del_vars_table_ea,
                                      NULL );
                g_hash_table_destroy( actualVars );
                actualVars = NULL;
        }

        /* set the balancing currency. */
        if ( commonCommodity == NULL ) {
                PERR( "Unable to find common currency/commodity." );
        } else {
                xaccTransSetCurrency( newT, commonCommodity );
        }

        {
                kvp_frame *txn_frame;
                /* set a kvp-frame element in the transaction indicating and
                 * pointing-to the SX this was created from. */
                txn_frame = xaccTransGetSlots( newT );
                if ( txn_frame == NULL ) {
                        txn_frame = kvp_frame_new();
                        xaccTransSetSlots_nc( newT, txn_frame );
                }
                kvp_val = kvp_value_new_guid( xaccSchedXactionGetGUID(tci->parentTCT->sx) );
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
create_transactions_on( SchedXaction *sx,
                        GDate *gd,
                        toCreateInstance *tci,
                        GList **createdGUIDs )
{
        createData createUD;
        AccountGroup *ag;
        Account *acct;
        const char *id;

        if ( tci ) {
                g_assert( g_date_compare( gd, tci->date ) == 0 );
        }

        ag = gnc_book_get_template_group( gnc_get_current_book () );
        id = guid_to_string( xaccSchedXactionGetGUID(sx) );
	if ( ag && id ) {
                acct = xaccGetAccountFromName( ag, id );
                if ( acct ) {
                        createUD.tci = tci;
                        createUD.createdGUIDs = createdGUIDs;
                        xaccAccountForEachTransaction( acct,
                                                       create_each_transaction_helper,
                                                       /*tct*/ &createUD );
                }
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
                const char *id;

                ag = gnc_book_get_template_group( gnc_get_current_book () );
                id = guid_to_string( xaccSchedXactionGetGUID(sx) );
                acct = xaccGetAccountFromName( ag, id );
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
                                parse_vars_from_formula( str, varHash, NULL );
                        }
                }

                kvp_val = kvp_frame_get_slot_path( kvpf,
                                                   GNC_SX_ID,
                                                   GNC_SX_DEBIT_FORMULA,
                                                   NULL);
                if ( kvp_val != NULL ) {
                        str = kvp_value_get_string( kvp_val );
                        if ( str && strlen(str) != 0 ) {
                                parse_vars_from_formula( str, varHash, NULL );
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

        /* FIXME: Next, deal with tab-ordering in this page...
         *
         * if ( entry isn't last in table )
         *    return (normal)FALSE
         * if ( unfilled entry in this table exists )
         *    change focus to unfilled entry
         * if ( no more unfilled clist-rows )
         *    return (normal)FALSE
         * clist-select next unfilled row
         *
         * This doesn't deal with shift-tab very well ... 
         * And there's a question of if the user will allow us to futz with
         * their tab-ordering... though it's already pretty screwed up for the
         * dynamically-changing-table anyways, so they probably won't mind
         * too much... -- jsled
         */

        return FALSE;
}

static
void
sxsincelast_tc_row_sel( GtkCTree *ct,
                        GList *nodelist,
                        gint column,
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
        GtkCTreeNode *node = GTK_CTREE_NODE( nodelist );

        toCreateInstance *tci;
        sxSinceLastData *sxsld;

        /* FIXME: this should more gracefully deal with multiple 'row-select'
         * signals from double/triple-clicks. */
        sxsld = (sxSinceLastData*)user_data;

        tci = (toCreateInstance*)gtk_ctree_node_get_row_data( ct, node );
        if ( !tci )
                return;

        sxsld->curSelTCI = tci;
        sxsld_set_sensitive_tci_controls( sxsld, TRUE );
        /* set real sensitivity based on the state of the TCI; when we change
         * the option menu selection here, it won't fire the selection-done
         * handler, so we have to force it. */

        {
                GtkOptionMenu *optMenu;

                optMenu = GTK_OPTION_MENU(
                        glade_xml_get_widget( sxsld->gxml,
                                              SX_DISPOSITION_OPT ) );
                gtk_option_menu_set_history( optMenu,
                                             sxsld->curSelTCI->state );
                sxsld_disposition_changed( GTK_MENU_SHELL(
                                                   gtk_option_menu_get_menu( optMenu ) ),
                                           sxsld );
        }

        /* Setup the query for the to-create register to only show the
         * transaction[s] associated with this lineitem. */
        {
                AccountGroup *ag;
                Account *acct;
                Query *q;
                const gchar *sxGUIDstr;
                SplitRegister *sr;

                q = xaccMallocQuery();
                xaccQuerySetBook( q, gnc_get_current_book() );
                ag = gnc_book_get_template_group( gnc_get_current_book() );
                sxGUIDstr = guid_to_string( xaccSchedXactionGetGUID( tci->parentTCT->sx ) );
                acct = xaccGetAccountFromName( ag, sxGUIDstr );
                g_assert( acct != NULL );
                xaccQueryAddSingleAccountMatch( q, acct, QUERY_AND );
          
                gnc_suspend_gui_refresh();
                gnc_ledger_display_set_query( sxsld->to_create_ledger, q );
                sr = gnc_ledger_display_get_split_register( sxsld->to_create_ledger );
                gnc_split_register_set_template_account( sr, acct );
                gnc_ledger_display_refresh( sxsld->to_create_ledger );
                gnc_resume_gui_refresh();
        }

        /* Get the count of variables; potentially remove the system-defined
         * variables if they're present in the expression. */
        varHashSize = g_hash_table_size( tci->varBindings );
        {
                gpointer *unusedKey, *unusedVal;
                if ( g_hash_table_lookup_extended( tci->varBindings, "i",
                                                   (gpointer)&unusedKey,
                                                   (gpointer)&unusedVal ) ) {
                        varHashSize -= 1;
                }
        }

        if ( varHashSize == 0 ) {
                return;
        }

        varList = NULL;
        hash_to_sorted_list( tci->varBindings, &varList );
        varTable = GTK_TABLE( glade_xml_get_widget( sxsld->gxml,
                                                    VARIABLE_TABLE ) );
        gtk_table_resize( varTable, varHashSize + 1, NUM_COLS );

        tableIdx = 1;
        for ( ; varList ; varList = varList->next ) {
                gchar *varName;
                GString *gstr;
		const gchar *numValueStr;
                gnc_numeric *numValue, *tmpNumValue;

                varName = (gchar*)varList->data;
                if ( strcmp( varName, "i" ) == 0 ) {
                        continue;
                }

                gstr = g_string_sized_new(16);
                g_string_sprintf( gstr, "%s: ", varName );
                label = gtk_label_new( gstr->str );
                gtk_label_set_justify( GTK_LABEL(label), GTK_JUSTIFY_RIGHT );
                g_string_free( gstr, TRUE );

                entry = gtk_entry_new();
                gtk_object_set_data( GTK_OBJECT(entry), "varName",
                                     varName );
                gtk_object_set_data( GTK_OBJECT(entry), "tci", tci );
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
                numValue = (gnc_numeric*)g_hash_table_lookup( tci->varBindings,
                                                              varName );
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
                gtk_signal_connect( GTK_OBJECT(entry), "destroy",
                                    GTK_SIGNAL_FUNC(gnc_sxsld_free_entry_numeric),
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
        GList *children, *toFree, *l;
        GtkTableChild *child;
        
        table = GTK_TABLE( glade_xml_get_widget( sxsld->gxml,
                                                 VARIABLE_TABLE ) );
        children = table->children;
        g_assert( children );

        toFree = NULL;
        for( ; children ; children = children->next ) {
                /* Destroy all children after the first [label-continaing]
                   row... ie, leave the labels in place. */
                child = (GtkTableChild*)children->data;
                if ( child->top_attach > 0 ) {
                        toFree = g_list_append( toFree, child->widget );
                }
        }

        gtk_table_resize( table, 1, 2 );

        for ( l = toFree; l; l = l->next ) {
                gtk_widget_destroy( (GtkWidget*)l->data );
        }
        g_list_free( toFree );
}

static void
sxsincelast_tc_row_unsel( GtkCTree *ct,
                          GList *nodelist,
                          gint column,
                          gpointer user_data)
{
        sxSinceLastData *sxsld;

        sxsld = (sxSinceLastData*)user_data;
        clean_variable_table( sxsld );

        sxsld->curSelTCI = NULL;

        sxsld_set_sensitive_tci_controls( sxsld, FALSE );

        {
                Query *q;
                q = xaccMallocQuery();
                xaccQueryClear( q );
                gnc_suspend_gui_refresh();
                gnc_ledger_display_set_query( sxsld->to_create_ledger, q );
                gnc_ledger_display_refresh( sxsld->to_create_ledger );
                gnc_resume_gui_refresh();
        }


        /* we cleanup the gnc_numerics we allocated in the "destroy" signal
         * handler of the entry [where we attached them] */
}

void
print_vars_helper( gpointer key, gpointer value, gpointer user_data )
{
        DEBUG( "\"%s\" -> %.8x [%s]",
               (gchar*)key, (unsigned int)value,
               gnc_numeric_to_string( *(gnc_numeric*)value ) );
}

int
parse_vars_from_formula( const char *formula,
                         GHashTable *varHash,
                         gnc_numeric *result )
{
        gnc_numeric *num;
        char *errLoc;
        int toRet;

        if ( result ) {
                num = result;
        } else {
                num = g_new0( gnc_numeric, 1 );
        }
        
        toRet = 0;
        if ( ! gnc_exp_parser_parse_separate_vars( formula, num,
                                                   &errLoc, varHash ) ) {
#if 0 /* just too verbose, as "Numeric errors" are acceptable in this
       * context. */
                PERR( "Error parsing at \"%s\": %s",
                        errLoc, gnc_exp_parser_error_string() );
#endif /* 0 */
                toRet = -1;
        }

        DEBUG( "result/num: %s", gnc_numeric_to_string( *num ) );
        if ( !result ) {
                g_free( num );
        }
        return toRet;
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
 * While we're doing this, we handle any previously-selected, now-unselected
 * reminders.
 *
 * Returns TRUE if there are processed, valid reminders... FALSE otherwise.
 **/
static gboolean
processed_valid_reminders_listP( sxSinceLastData *sxsld )
{
        reminderTuple *rt;
        reminderInstanceTuple *rit;
        char *rtName;
        gboolean overallOkFlag, okFlag, prevState;
        GList *reminderList;
        GList *reminderInstList;
        GList *badList;
        GList *badRecentRun;
        GList *goodList;
        GList *toRevertList;

        rtName = NULL;
        goodList = NULL;
        overallOkFlag = TRUE;

        okFlag = prevState = TRUE;
        badList = badRecentRun = NULL;
        rt = NULL;
        toRevertList = NULL;

        for ( reminderList = sxsld->reminderList;
              reminderList;
              reminderList = reminderList->next ) {

                rt = (reminderTuple*)reminderList->data;
                okFlag = prevState = TRUE;
                badList = badRecentRun = NULL;
                rtName = xaccSchedXactionGetName( rt->sx );

                for ( reminderInstList = rt->instanceList;
                      reminderInstList;
                      reminderInstList = reminderInstList->next ) {
                        rit = (reminderInstanceTuple*)reminderInstList->data;

                        /* If we've previously created this RIT and now it's
                         * not selected, then prepare to revert it
                         * [later]. */
                        if ( !rit->isSelected
                             && rit->resultantTCI ) {
                                toRevertList = g_list_append( toRevertList, rit );
                        }

                        if ( prevState ) {
                                prevState = rit->isSelected;
                                if ( !prevState ) {
                                        badRecentRun = g_list_append( badRecentRun, rit );
                                }
                        } else {
                                if ( rit->isSelected ) {
                                        okFlag = FALSE;
                                        if ( g_list_length( badRecentRun ) > 0 ) {
                                                badList = g_list_concat( badList,
                                                                         badRecentRun );
                                                badRecentRun = NULL;
                                        }
                                } else {
                                        badRecentRun =
                                                g_list_append( badRecentRun, rit );
                                }
                        }
                }
                overallOkFlag &=
                        inform_or_add( sxsld, rt, okFlag, badList, &goodList );
                if ( badList ) {
                        g_list_free( badList );
                        badList = NULL;
                }
                if ( badRecentRun ) {
                        g_list_free( badRecentRun );
                        badRecentRun = NULL;
                }
        }

        /* Handle implications of above logic. */
        if ( !overallOkFlag ) {
                g_list_free( goodList );
                goodList = NULL;

                g_list_free( toRevertList );
                toRevertList = NULL;

                return FALSE;
        }

        if ( g_list_length( goodList ) > 0 ) {
                processSelectedReminderList( goodList, sxsld );
                g_list_free( goodList );
                goodList = NULL;
        }

        /* Revert the previously-created and now-unselected RITs. */
        gnc_sxsld_revert_reminders( sxsld, toRevertList );
        g_list_free( toRevertList );
        toRevertList = NULL;

        return TRUE;
}


/**
 * Remove the TCI from it's parent TCT, deleting any created transactions as
 * appropriate. Note: if after removing a TCIs from it's TCT and there are no
 * more instances in the TCT, then the TCT wouldn't have been created except
 * for us, and should be removed itself; we handle this as well.
 **/
static
void
gnc_sxsld_revert_reminders( sxSinceLastData *sxsld,
                            GList *toRevertList )
{
        reminderInstanceTuple *rit;
        toCreateInstance *tci;
        toCreateTuple *tct;
        gboolean autoCreateState, notifyState;
        GList *l, *m;

        if ( !toRevertList ) {
                return;
        }

        for ( l = toRevertList; l; l = l->next ) {
                /* Navigate to the relevant objects. */
                rit = (reminderInstanceTuple*)l->data;
                g_assert( rit );
                tci = rit->resultantTCI;
                g_assert( tci );
                tct = tci->parentTCT;
                g_assert( tct );

                tct->instanceList = g_list_remove( tct->instanceList, tci );

                if ( g_list_length(tct->instanceList) == 0 ) {
                        GList **correctList;
                        /* if there are no instances, remove the TCT as
                         * well. */
                        xaccSchedXactionGetAutoCreate( rit->parentRT->sx,
                                                       &autoCreateState,
                                                       &notifyState );
                        correctList = NULL;
                        if ( autoCreateState ) {
                                if ( notifyState ) {
                                        correctList = &sxsld->autoCreateList;
                                }
                        } else {
                                correctList = &sxsld->toCreateList;
                        }

                        if ( correctList ) 
                                *correctList = g_list_remove( *correctList, tct );
                }

                /* destroy any created transactions. */
                gnc_suspend_gui_refresh();
                for ( m = tci->createdTxnGUIDs; m; m = m->next ) {
                        Transaction *t;

                        sxsld->createdTxnGUIDList =
                                g_list_remove( sxsld->createdTxnGUIDList,
                                               (GUID*)m->data );
                        t = xaccTransLookup( (GUID*)m->data,
                                             gnc_get_current_book() );
                        g_assert( t );
                        xaccTransBeginEdit(t);
                        xaccTransDestroy(t);
                        xaccTransCommitEdit(t);

                }
                gnc_resume_gui_refresh();

                /* Free the now-dead TCI; this is buggy and causing
                 * problems... */
                gnc_sxsld_free_tci( tci );
                rit->resultantTCI = NULL;
        }
}


static void
sxsld_remind_row_toggle( GtkCTree *ct, GList *node,
                         gint column, gpointer user_data )
{
        GtkCTreeNode *ctn;
        reminderInstanceTuple *rit;
        sxSinceLastData *sxsld = (sxSinceLastData*)user_data;
        GnomeDruidPage *thisPage, *nextPage;

        ctn = GTK_CTREE_NODE( node );
        rit = (reminderInstanceTuple*)gtk_ctree_node_get_row_data( ct, ctn );
        if ( rit == NULL ) {
                PERR( "We got called to toggle a row that "
                      "we can't find data for..." );
                return;
        }
        rit->isSelected = !rit->isSelected;

        /* Deal with setting up a correct next/finish button for this
         * page. */
        sxsld->remindSelCount += ( rit->isSelected ? 1 : -1 );
        thisPage = GNOME_DRUID_PAGE(glade_xml_get_widget( sxsld->gxml, REMINDERS_PG ));
        nextPage = gnc_sxsld_get_appropriate_page( sxsld, thisPage, FORWARD );
        if ( sxsld->remindSelCount == 0
             || sxsld->remindSelCount == 1 ) {
                /* If we don't have anywhere to go [read: there's only
                 * reminders as of yet], and we've selected no reminders. */

                /* FIXME: damnit, this won't work correctly as we really want
                 * to incorporate the effect of changing the reminder
                 * selections into this, too. */

                gnome_druid_set_show_finish( sxsld->sincelast_druid,
                                             ( !nextPage
                                               && (sxsld->remindSelCount == 0) ) );

        } /* else { This else intentionally left blank; if it's >1, then we
           * handled the 'next/finish' button on the 0 -> 1 transition. } */
}

static
void
sxsld_obsolete_row_toggle( GtkCList *cl, gint row, gint col,
                           GdkEventButton *event, gpointer ud )
{
        toDeleteTuple *tdt;

        tdt = (toDeleteTuple*)gtk_clist_get_row_data( cl, row );
        tdt->isSelected = !tdt->isSelected;
}

/**
 * @return the count of created transactions which would be true after
 * processing the currently-selected state of to-create transactions.  Note
 * that this includes auto-created transactions, which aren't shown in the
 * post-to-create review page.
 **/
static
gint
sxsld_get_future_created_txn_count( sxSinceLastData *sxsld )
{
        GList *tctList, *tciList;
        /* Get a reasonable initial count to modify below. */
        gint toRet = g_list_length( sxsld->createdTxnGUIDList );

        for ( tctList = sxsld->toCreateList;
              tctList; tctList = tctList->next ) {

                toCreateTuple *tct = (toCreateTuple*)tctList->data;

                for ( tciList = tct->instanceList;
                      tciList;
                      tciList = tciList->next ) {

                        GList *txnSet, *splitList;
                        toCreateInstance *tci = (toCreateInstance*)tciList->data;

                        if ( tci->state == tci->prevState ) {
                                continue;
                        }
                        
                        switch ( tci->state ) {
                        case TO_CREATE:
                                /* We were postpone or ignore, before ... so
                                 * add the new txns in. */

                                /* Calculate the size of the transaction-list to be created. */
                                txnSet = NULL;
                                splitList = xaccSchedXactionGetSplits( tci->parentTCT->sx );
                                for ( ; splitList; splitList = splitList->next ) {
                                        Split *s = (Split*)splitList->data;
                                        if ( g_list_find( txnSet, xaccSplitGetParent(s) ) == NULL ) {
                                                txnSet = g_list_append( txnSet, (gpointer)s );
                                        }
                                }
                                toRet += g_list_length( txnSet );
                                g_list_free( txnSet );
                                txnSet = NULL;
                                break;
                        case IGNORE:
                        case POSTPONE:
                                /* We were {postpone,ignore} or to-create,
                                 * before, so either continue to ignore or
                                 * subtract out the txns. */
                                if ( tci->prevState != TO_CREATE ) {
                                        continue;
                                }
                                toRet -= g_list_length( tci->createdTxnGUIDs );
                                break;
                        case UNDEF:
                        case MAX_STATE:
                                g_assert( "We shouldn't see any of these." );
                                break;
                        }
                }
        }
        g_assert( toRet >= 0 );
        return toRet;
}

static
void
sxsld_disposition_changed( GtkMenuShell *b, gpointer d )
{
        sxSinceLastData *sxsld = (sxSinceLastData*)d;
        ToCreateState newState;
        gboolean newSensitivity;
        GtkCTree *ct;
        char *newCtreeText;

        newState =
                gnc_option_menu_get_active( 
                        glade_xml_get_widget( sxsld->gxml,
                                              SX_DISPOSITION_OPT ));
        /* Change the state of the TCI */
        g_assert( sxsld->curSelTCI != NULL );
        sxsld->curSelTCI->state = newState;

        newSensitivity = TRUE;
        newCtreeText = "FIXME";

        switch ( newState ) {
        case TO_CREATE:
                newSensitivity = TRUE;
                {
                        gboolean allVarsBound = TRUE;
                        /* If there are no un-bound variables, then set the 'ready-to-go'
                           flag to 'y'. */
                        g_hash_table_foreach( sxsld->curSelTCI->varBindings,
                                              andequal_numerics_set,
                                              &allVarsBound );
                        newCtreeText = ( allVarsBound
                                         ? _( READY_TEXT )
                                         : _( NEEDS_BINDINGS_TEXT ) );
                }
                break;
        case IGNORE:
                newSensitivity = FALSE;
                newCtreeText = _( IGNORE_TEXT );
                break;
        case POSTPONE:
                newSensitivity = FALSE;
                newCtreeText = _( POSTPONE_TEXT );
                break;
        default:
                g_assert( FALSE );
                break;
        }

        gtk_widget_set_sensitive( glade_xml_get_widget( sxsld->gxml,
                                                        VARIABLE_TABLE ),
                                  newSensitivity );
        ct = GTK_CTREE(glade_xml_get_widget( sxsld->gxml, TO_CREATE_LIST ));
        gtk_ctree_node_set_text( ct, sxsld->curSelTCI->node, 1, newCtreeText );

        /* set the 'next/finish' button appropraitely based on the new
         * selection. */
        gnome_druid_set_show_finish( sxsld->sincelast_druid,
                                     ( ( sxsld_get_future_created_txn_count(sxsld)
                                         - sxsld->autoCreatedCount )== 0 ) );
}

/**
 * Makes both the variable table and disposition selection [in]sensitive, as
 * specified.
 **/
static
void
sxsld_set_sensitive_tci_controls( sxSinceLastData *sxsld,
                                  gboolean sensitive )
{
        GtkWidget *w;
        w = glade_xml_get_widget( sxsld->gxml, SX_DISPOSITION_OPT );
        gtk_widget_set_sensitive( w, sensitive );
        w = glade_xml_get_widget( sxsld->gxml, VARIABLE_TABLE );
        gtk_widget_set_sensitive( w, sensitive );
}

static void
create_bad_reminders_msg( gpointer data, gpointer ud )
{
        GString *msg;
        reminderInstanceTuple *rit;
        static char tmpBuf[ MAX_DATE_LENGTH+1 ];

        rit = (reminderInstanceTuple*)data;
        msg = (GString*)ud;
        printGDate( tmpBuf, rit->occurDate );
        g_string_sprintfa( msg, tmpBuf );
        g_string_sprintfa( msg, "\n" );
}

static gboolean
inform_or_add( sxSinceLastData *sxsld, reminderTuple *rt, gboolean okFlag,
               GList *badList, GList **goodList )
{
        reminderInstanceTuple *rit;
        GList *instances;
        GString *userMsg;

        userMsg = NULL;

        if ( okFlag ) {
                /* Add selected instances of this rt to
                 * okay-to-add-to-toCreateList list. */
                for ( instances = rt->instanceList;
                      instances;
                      instances = instances->next ) {
                        rit = (reminderInstanceTuple*)instances->data;
                        /* this isn't really all that efficient. */
                        if ( rit->isSelected ) {
                                *goodList = g_list_append( *goodList, rit );
                        }
                }
        } else {
                /* [Add to list for later] dialog issuance to user. */

                userMsg = g_string_sized_new( 128 );
                g_string_sprintf( userMsg,
                                  "You cannot skip instances of "
                                  "Scheduled Transactions.\n"
                                  "The following instances of \"%s\"\n"
                                  "must be selected as well:\n\n",
                                  xaccSchedXactionGetName( rt->sx ) );
                g_list_foreach( badList, create_bad_reminders_msg, userMsg );
                gnc_error_dialog( sxsld->sincelast_window, userMsg->str );
                g_string_free( userMsg, TRUE );
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
	xaccQuerySetBook (q, gnc_get_current_book ());
        sxsld->ac_ledger = gnc_ledger_display_query( q,
                                                     GENERAL_LEDGER,
                                                     REG_STYLE_LEDGER );
        splitreg = gnc_ledger_display_get_split_register( sxsld->ac_ledger );

        /* FIXME: Make numRows configurable. */
        sxsld->ac_gsr =
                GNC_SPLIT_REG(
                        gnc_split_reg_new( sxsld->ac_ledger,
                                           GTK_WINDOW( sxsld->sincelast_window ),
                                           4,
                                           (CREATE_TOOLBAR | CREATE_POPUP),
                                           CAP_SCHEDULE ) );
        vbox = glade_xml_get_widget( sxsld->gxml, AUTO_CREATE_VBOX );
        toolbar = gnc_split_reg_get_toolbar( sxsld->ac_gsr );

        gtk_box_pack_start( GTK_BOX(vbox), toolbar, FALSE, FALSE, 2 );
        gtk_box_pack_end( GTK_BOX(vbox), GTK_WIDGET(sxsld->ac_gsr), TRUE, TRUE, 2 );
        gnc_split_reg_use_extended_popup( sxsld->ac_gsr );
        gnc_split_register_config(splitreg,
                                  splitreg->type, splitreg->style,
                                  FALSE);
        gnc_split_register_show_present_divider( splitreg, FALSE );
        gnc_ledger_display_refresh( sxsld->ac_ledger );
}

static void
create_created_ledger( sxSinceLastData *sxsld )
{
        SplitRegister *splitreg;
        GtkWidget *vbox, *toolbar;
        Query *q;

        q = xaccMallocQuery();
	xaccQuerySetBook (q, gnc_get_current_book ());
        sxsld->created_ledger = gnc_ledger_display_query( q,
                                                          GENERAL_LEDGER,
                                                          REG_STYLE_LEDGER );
        splitreg = gnc_ledger_display_get_split_register( sxsld->created_ledger );
        /* FIXME: make numRows configurable? */
        sxsld->created_gsr =
                GNC_SPLIT_REG( 
                        gnc_split_reg_new( sxsld->created_ledger,
                                           GTK_WINDOW( sxsld->sincelast_window ),
                                           4,
                                           ( CREATE_TOOLBAR | CREATE_POPUP ),
                                           CAP_SCHEDULE ) );

        vbox = glade_xml_get_widget( sxsld->gxml, CREATED_VBOX );
        toolbar = gnc_split_reg_get_toolbar( sxsld->created_gsr );

        gtk_box_pack_start( GTK_BOX(vbox), toolbar, FALSE, FALSE, 2 );
        gtk_box_pack_end( GTK_BOX(vbox), GTK_WIDGET(sxsld->created_gsr), TRUE, TRUE, 2 );

        gnc_split_reg_use_extended_popup( sxsld->created_gsr );
        gnc_split_register_config(splitreg,
                                  splitreg->type, splitreg->style,
                                  FALSE);
        gnc_split_register_show_present_divider( splitreg, FALSE );
        gnc_ledger_display_refresh( sxsld->created_ledger );
}

static
void
sxsld_jump_to_real_txn( GNCSplitReg *gsr, gpointer ud )
{
        SplitRegister *reg;
        Account *account;
        Account *leader;
        Split *split;
        sxSinceLastData *sxsld = (sxSinceLastData*)ud;
        reg = gnc_ledger_display_get_split_register( sxsld
                                                     ->to_create_ledger );

        split = gnc_split_register_get_current_split (reg);
        if (split == NULL)
                return;

        {
                GUID *acct_guid;
                kvp_frame *split_kvpf;
                kvp_value *kvp_val;
                
                split_kvpf = xaccSplitGetSlots( split );
                kvp_val = kvp_frame_get_slot_path( split_kvpf,
                                                   GNC_SX_ID,
                                                   GNC_SX_ACCOUNT,
                                                   NULL );
                if ( kvp_val == NULL ) {
                        PERR( "Null kvp_val for account" );
                }
                acct_guid = kvp_value_get_guid( kvp_val );
                account = xaccAccountLookup( acct_guid,
                                             gnc_get_current_book ());
        }
        
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
                        gsr = regWindowSimple( account );
                }
                gnc_split_reg_raise( gsr );
                gnc_split_reg_jump_to_split( gsr, split );
        }
        
        gtk_signal_emit_stop_by_name( GTK_OBJECT(gsr), "jump" );
}

static void
create_to_create_ledger( sxSinceLastData *sxsld )
{
        SplitRegister *splitreg;
        GtkWidget *txn_reg_frame;
        Query *q;

        sxsld->to_create_ledger = gnc_ledger_display_template_gl( NULL );
        q = xaccMallocQuery();
        xaccQueryClear( q );
        gnc_ledger_display_set_query( sxsld->to_create_ledger, q );
        splitreg = gnc_ledger_display_get_split_register( sxsld->to_create_ledger );
        /* FIXME: make numRows configurable? */
        sxsld->to_create_gsr =
                GNC_SPLIT_REG( 
                        gnc_split_reg_new( sxsld->to_create_ledger,
                                           GTK_WINDOW( sxsld->sincelast_window ),
                                           4,
                                           ( CREATE_TOOLBAR | CREATE_POPUP ),
                                           ( CAP_READ_ONLY | CAP_SCHEDULE) ) );

        gtk_signal_connect( GTK_OBJECT(sxsld->to_create_gsr), "jump",
                            GTK_SIGNAL_FUNC( sxsld_jump_to_real_txn ),
                            sxsld );

        txn_reg_frame = glade_xml_get_widget( sxsld->gxml, TO_CREATE_TXN_REG_FRAME );
        gtk_container_add( GTK_CONTAINER( txn_reg_frame ),
                           GTK_WIDGET( sxsld->to_create_gsr ) );
        gnc_split_register_config( splitreg, splitreg->type, splitreg->style,
                                   FALSE );
        gnc_split_register_show_present_divider( splitreg, FALSE );
        gnc_ledger_display_refresh( sxsld->to_create_ledger );
}

static void
clean_sincelast_data( sxSinceLastData *sxsld )
{
        GList *l, *m;

        /* FIXME: handle the cloned state data for all the instance
         * structures. */

        /* Free the reminder list */
        for ( l = sxsld->reminderList; l; l = l->next ) {
                reminderTuple *rt;
                reminderInstanceTuple *rit;

                rt = (reminderTuple*)l->data;
                for ( m = rt->instanceList; m; m = m->next ) {
                        rit = (reminderInstanceTuple*)m->data;
                        g_date_free( rit->endDate );
                        g_date_free( rit->occurDate );
                        g_free( rit );
                }
                g_list_free( rt->instanceList );
                rt->instanceList = NULL;
                g_free( rt );
        }
        g_list_free( sxsld->reminderList );

        /* Free the auto-create and to-create lists */
        gnc_sxsld_free_toCreateTuple_list( sxsld->autoCreateList );
        g_list_free( sxsld->autoCreateList );
        sxsld->autoCreateList = NULL;

        gnc_sxsld_free_toCreateTuple_list( sxsld->toCreateList );
        g_list_free( sxsld->toCreateList );
        sxsld->toCreateList = NULL;

        /* Free the to-remove list */
        for ( l = sxsld->toRemoveList; l; l = l->next ) {
                toDeleteTuple *tdt;

                tdt = (toDeleteTuple*)l->data;
                g_date_free( tdt->endDate );
                tdt->endDate = NULL;

                g_free( tdt );
        }
        g_list_free( sxsld->toRemoveList );
        sxsld->toRemoveList = NULL;

        /* free the created-txn-guid list */
        g_list_free( sxsld->createdTxnGUIDList );
        sxsld->createdTxnGUIDList = NULL;

        /* Free the saved SX temporal states */
        g_hash_table_foreach( sxsld->sxInitStates,
                              gnc_sxsld_free_sxState,
                              NULL );
        g_hash_table_destroy( sxsld->sxInitStates );
        sxsld->sxInitStates = NULL;

}

static
void
gnc_sxsld_free_tci( toCreateInstance *tci )
{
        if ( tci->date ) {
                g_date_free(tci->date);
                tci->date = NULL;
        }

        if ( tci->varBindings ) {
                g_hash_table_foreach( tci->varBindings,
                                      _free_varBindings_hash_elts,
                                      NULL );
                g_hash_table_destroy( tci->varBindings );
                tci->varBindings = NULL;
        }

        /* Handling these original/previous/current-stated things is painful,
         * but here's the rules...
         *
         * If we're not cancelling...
         * . If ignored, destroy.
         * . If postponed, DON'T destroy.
         * . If to-create, destroy.
         *
         * If we are cancelling...
         * . If ignored, destroy.
         * . If postponed, destroy.
         *   . UNLESS previously postponed
         * . If to-create, destroy.
         *
         * So, we don't destroy postponed by default, and let the
         * cancel-specific case handle that destruction [thus the
         * valid-pointer check].
         */
        if ( tci->prevState      != POSTPONE
             && tci->origState   != POSTPONE
             && tci->sxStateData != NULL ) {
                gnc_sx_destroy_temporal_state( tci->sxStateData );
                tci->sxStateData = NULL;
        }

        tci->parentTCT = NULL;

        if ( tci->createdTxnGUIDs ) {
                g_list_free( tci->createdTxnGUIDs );
                tci->createdTxnGUIDs = NULL;
        }

        g_free( tci );
}

/**
 * Frees a list of toCreateTuples, like the autoCreateList and
 * toCreateList.
 **/
static
void
gnc_sxsld_free_toCreateTuple_list( GList *l )
{
        GList *m;
        toCreateTuple *tct;

        for ( ; l; l = l->next ) {
                tct = (toCreateTuple*)l->data;
                for ( m = tct->instanceList; m; m = m->next ) {
                        gnc_sxsld_free_tci( (toCreateInstance*)m->data );
                }
                g_list_free( tct->instanceList );
                tct->instanceList = NULL;
                g_free( tct );
        }
}

static
void
gnc_sxsld_free_sxState( gpointer key,
                        gpointer value,
                        gpointer userdata )
{
        gnc_sx_destroy_temporal_state( (void*)value );
}

static
void
gnc_sxsld_free_entry_numeric( GtkObject *o, gpointer ud )
{
        gnc_numeric *num;
        num = (gnc_numeric*)gtk_object_get_data( o, "numeric" );
        g_free( num );
}

static
void
gnc_sxsld_commit_ledgers( sxSinceLastData *sxsld )
{
        gnc_split_register_save(
                gnc_ledger_display_get_split_register(sxsld->created_ledger),
                TRUE );
        gnc_split_register_save(
                gnc_ledger_display_get_split_register(sxsld->ac_ledger),
                TRUE );
}

