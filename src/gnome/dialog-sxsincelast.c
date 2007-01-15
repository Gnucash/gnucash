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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
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

#include <gnome.h>
#include <glib.h>
#include <glib/gi18n.h>
#include "glib-compat.h"
#include <limits.h>

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
#include "gnc-date.h"
#include "gnc-component-manager.h"
#include "gnc-engine.h"
#include "gnc-exp-parser.h"
#include "gnc-embedded-window.h"
#include "gnc-gconf-utils.h"
#include "gnc-main-window.h"
#include "gnc-numeric.h"
#include "gnc-plugin-page.h"
#include "gnc-plugin-page-register.h"
#include "gnc-ui-util.h"
#include "gnc-ui.h"
#include "gnc-gui-query.h"
#include "split-register.h"
#include "gnc-ledger-display.h"
#include "gnucash-sheet.h"
#include "gnc-split-reg.h"

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
#define GCONF_SECTION "dialogs/scheduled_trans/since_last_run"

#define SINCELAST_DRUID   "sincelast_druid"
#define WHAT_TO_DO_PG "what_to_do"
#define REMINDERS_PG "reminders_page"
#define AUTO_CREATE_NOTIFY_PG "auto_create_notify_page"
#define TO_CREATE_PG "to_create_page"
#define CREATED_PG "created_page"
#define OBSOLETE_PG "obsolete_page"
#define COMMIT_PG "commit_page"

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
#define TO_CREATE_TXN_VBOX "to_create_txn_vbox"
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

static QofLogModule log_module = GNC_MOD_SX;

/**
 * Directions for {forward,back}-page determining.
 * @see gnc_sxsld_get_appropriate_page
 **/
typedef enum {
        FORWARD, BACK
} Direction;

/**
 * The states a to-be-created SX can be in...
 * SX_TO_CREATE : The SX is ready to be created, depending on variable-binding
 *               requirements.
 * SX_IGNORE   : Drop the SX on the floor forever.
 * SX_POSTPONE : Bring this SX up in the future, but we're not going to
 *               create it right now.
 * SX_[MAX_STATE] : The maximum real value.
 * SX_UNDEF     : Only used for prevState, to indicate that we haven't
 *               processed this instance, yet.
 **/
typedef enum {
        SX_TO_CREATE,
        SX_IGNORE,
        SX_POSTPONE,
        SX_MAX_STATE,
        SX_UNDEF
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
        GDate        *endDate;
        GDate        *occurDate;
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
        /* a pointer to a GList to append the GUIDs of newly-created
         * Transactions to, or NULL */
        GList **createdGUIDs;
        /* a pointer to a GList<GString*> of error-messages encountered while
         * creating the transactions. **/
        GList **creation_errors;
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

	GncEmbeddedWindow   *ac_window;
	GncPluginPage       *ac_register;
        GNCLedgerDisplay    *ac_ledger;

	GncEmbeddedWindow   *created_window;
	GncPluginPage       *created_register;
        GNCLedgerDisplay    *created_ledger;

	GncEmbeddedWindow   *to_create_window;
	GncPluginPage       *to_create_register;
        GNCLedgerDisplay    *to_create_ledger;

} sxSinceLastData;

static void sxsincelast_init( sxSinceLastData *sxsld );
static void create_autoCreate_ledger( sxSinceLastData *sxsld );
static void create_created_ledger( sxSinceLastData *sxsld );
static void create_to_create_ledger( sxSinceLastData *sxsld );
static void gnc_sxsld_commit_ledgers( sxSinceLastData *sxsld );

#if 0
static void sxsld_jump_to_real_txn( GtkAction *action, sxSinceLastData *sxsld );
#endif

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
static gboolean gnc_sxsld_commit_appr( sxSinceLastData *sxsld );

static void sxsincelast_entry_changed( GtkEditable *e, gpointer ud );
static void sxsincelast_destroy( GtkObject *o, gpointer ud );
static void sxsincelast_save_size( sxSinceLastData *sxsld );
static void create_transactions_on(SchedXaction *sx,
                                   GDate *gd,
                                   toCreateInstance *tci,
                                   GList **createdGUIDs,
                                   GList **creation_errors);
static gint create_each_transaction_helper( Transaction *t, void *d );
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

static void process_auto_create_list(GList *, sxSinceLastData *sxsld, GList **creation_errors);
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
static void gnc_sxsld_free_entry_numeric( GObject *o, gpointer ud );

static gint sxsld_process_to_create_instance(sxSinceLastData *sxsld,
                                             toCreateInstance *tci,
                                             GList **creation_errors);
static void sxsld_revert_to_create_txns( sxSinceLastData *sxsld,
                                         toCreateInstance *tci );
static gint sxsld_create_to_create_txns(sxSinceLastData *sxsld,
                                        toCreateInstance *tci,
                                        GList **creation_errors);
static gint sxsld_get_future_created_txn_count( sxSinceLastData *sxsld );
static void creation_errors_dialog(GList *creation_errors);
static void creation_errors_free(GList *creation_errors);

static GtkActionEntry gnc_sxsld_menu_entries [] =
{
	/* Toplevel */
	{ "EditAction", NULL, N_("_Edit"), NULL, NULL, NULL },
	{ "TransactionAction", NULL, N_("_Transaction"), NULL, NULL, NULL },
	{ "ViewAction", NULL, N_("_View"), NULL, NULL, NULL },
	{ "ActionsAction", NULL, N_("_Actions"), NULL, NULL, NULL },
};
static guint gnc_sxsld_menu_n_entries = G_N_ELEMENTS (gnc_sxsld_menu_entries);

/**
 * Used to wrap for the book-open hook, where the book filename is given.
 **/
void
gnc_sx_sxsincelast_book_opened (void)
{
  gint ret;

  if (!gnc_gconf_get_bool(GCONF_SECTION, "show_at_file_open", NULL))
    return;

  ret = gnc_ui_sxsincelast_dialog_create();
  if ( ret < 0 ) {
    gnc_info_dialog
      (NULL,
       ngettext 
       ("There are no Scheduled Transactions to be entered at this time. "
        "(%d transaction automatically created)",
        "There are no Scheduled Transactions to be entered at this time. "
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

        for (i = 0; handler_info[i].name != NULL; i++)
        {
                w = glade_xml_get_widget(dialog_xml, handler_info[i].name);
                g_signal_connect( G_OBJECT(w), handler_info[i].signal, 
				  G_CALLBACK(handler_info[i].handlerFn),
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
                        g_signal_connect( G_OBJECT(w), "prepare",
					  G_CALLBACK(handler_info[i].
						     prepareHandlerFn),
					  sxsld);
                }
                if ( handler_info[i].backHandlerFn ) {
                        g_signal_connect( G_OBJECT(w), "back",
					  G_CALLBACK(handler_info[i].
						     backHandlerFn),
					  sxsld);
                }
                if ( handler_info[i].nextHandlerFn ) {
                        g_signal_connect( G_OBJECT(w), "next",
					  G_CALLBACK(handler_info[i].
						     nextHandlerFn),
					  sxsld);
                }
                if ( handler_info[i].finishHandlerFn ) {
                        g_signal_connect( G_OBJECT(w), "finish",
					  G_CALLBACK(handler_info[i].
						     finishHandlerFn),
					  sxsld);
                }
                if ( handler_info[i].cancelHandlerFn ) {
                        g_signal_connect( G_OBJECT(w), "cancel",
					  G_CALLBACK(handler_info[i].
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
                { COMMIT_PG,             gnc_sxsld_commit_appr },
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
gnc_sxsld_commit_appr( sxSinceLastData *sxsld )
{
	/* Always show this page */
        return TRUE;
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
                TRUE, TRUE, TRUE );
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
                TRUE, TRUE, TRUE );

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
                TRUE, TRUE, TRUE );
}

static void
commit_prep( GnomeDruidPage *druid_page,
	     gpointer arg1, gpointer ud )
{
        sxSinceLastData *sxsld = (sxSinceLastData*)ud;

        gnome_druid_set_buttons_sensitive(
                sxsld->sincelast_druid,
                ( gnc_sxsld_get_appropriate_page( sxsld,
                                                  druid_page,
                                                  BACK )
                  != NULL ),
                TRUE, TRUE, TRUE );
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
                TRUE, TRUE, TRUE );

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
                TRUE, TRUE, TRUE );
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
sxsld_create_to_create_txns(sxSinceLastData *sxsld,
                            toCreateInstance *tci,
                            GList **creation_errors)
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

        create_transactions_on(tci->parentTCT->sx,
                               tci->date,
                               tci,
                               &created,
                               creation_errors);
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
sxsld_process_to_create_instance(sxSinceLastData *sxsld,
                                 toCreateInstance *tci,
                                 GList **creation_errors)
{
        gint toRet = 0;

        /* Undo the previous work. */
        switch ( tci->prevState ) {
        case SX_IGNORE:
                switch ( tci->state ) {
                case SX_IGNORE:
                        /* Keep ignoring. */
                        break;
                case SX_POSTPONE:
                        /* remove from postponed list. */
                        gnc_sx_remove_defer_instance( tci->parentTCT->sx,
                                                      tci->sxStateData );
                        break;
                case SX_TO_CREATE:
                        /* del prev txns. */
                        sxsld_revert_to_create_txns( sxsld, tci );
                        break;
                default:
                        g_assert( FALSE );
                }
                break;
        case SX_POSTPONE:
                if ( tci->state != SX_POSTPONE ) {
                        /* remove from postponed list. */
                        gnc_sx_remove_defer_instance( tci->parentTCT->sx,
                                                      tci->sxStateData );
                }
                break;
        case SX_TO_CREATE:
                if ( tci->state != SX_TO_CREATE ) {
                        /* del prev txns. */
                        sxsld_revert_to_create_txns( sxsld, tci );
                }
                break;
        case SX_UNDEF:
                /* Fine; do nothing. */
                break;
        default:
                g_assert( FALSE );
                break;
        }

        /* Now, process the currently-requested state. */
        switch ( tci->state ) {
        case SX_IGNORE:
                /* Fine ... just ignore it. */
                break;
        case SX_POSTPONE:
                if ( tci->prevState == SX_POSTPONE ) {
                        break;
                }
                /* add to the postponed list. */
                { 
                        char tmpBuf[ MAX_DATE_LENGTH+1 ];
                        qof_print_gdate( tmpBuf, MAX_DATE_LENGTH, tci->date );
                        DEBUG( "Adding defer instance on %s for %s",
                               tmpBuf,
                               xaccSchedXactionGetName( tci->parentTCT->sx ) );
                }
                gnc_sx_add_defer_instance( tci->parentTCT->sx, tci->sxStateData );
                break;
        case SX_TO_CREATE:
                /* Go ahead and create... */
                toRet = sxsld_create_to_create_txns(sxsld, tci, creation_errors);
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
                if ( tci->origState == SX_UNDEF ) {
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
        GList *tcList, *tcInstList, *creation_errors;
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

                        if ( tci->state == SX_IGNORE
                             || tci->state == SX_POSTPONE ) {
                                continue;
                        }

                        allVarsBound = TRUE;
                        g_hash_table_foreach( tci->varBindings,
                                              andequal_numerics_set,
                                              &allVarsBound );
                        if ( !allVarsBound ) {
                                char tmpBuf[ MAX_DATE_LENGTH+1 ];
                                qof_print_gdate( tmpBuf, MAX_DATE_LENGTH, tci->date );
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

        creation_errors = NULL;
        gnc_suspend_gui_refresh();
        for ( ; tcList ; tcList = tcList->next ) {
                tct = (toCreateTuple*)tcList->data;

                for ( tcInstList = tct->instanceList;
                      tcInstList;
                      tcInstList = tcInstList->next ) {

                        tci = (toCreateInstance*)tcInstList->data;
                        sxsld_process_to_create_instance(sxsld, tci, &creation_errors);
                }
        }
        gnc_resume_gui_refresh();
        if (g_list_length(creation_errors) > 0)
        {
                creation_errors_dialog(creation_errors);
                creation_errors_free(creation_errors);
        }
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

                        row = GPOINTER_TO_INT(toDelPtr->data);
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
          _( "Canceling the Since-Last-Run dialog "
             "will revert all changes. "
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
                                if ( tci->prevState == SX_POSTPONE
                                     && tci->origState    != SX_POSTPONE ) {
                                        /* Any valid [non-null] 'prevState !=
                                         * SX_POSTPONE' sx temporal state
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
        GObject *o;
        GnomeDruidPage *nextPage;
        GList *creation_errors;
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

                { COMMIT_PG,
                  commit_prep, gen_back, gen_next,
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

        g_signal_connect( G_OBJECT(sxsld->sincelast_window), "destroy",
			  G_CALLBACK( sxsincelast_destroy ), sxsld );

        dialog_widgets_attach_handlers(sxsld->gxml, widgets, sxsld);
        druid_pages_attach_handlers( sxsld->gxml, pages, sxsld );

        /* gnc-init the option menu[s]. */
        for ( i=0; optionMenus[i].name != NULL; i++ ) {
                w = glade_xml_get_widget( sxsld->gxml, optionMenus[i].name );
                gnc_option_menu_init( w );
                o = G_OBJECT(gtk_option_menu_get_menu(GTK_OPTION_MENU(w)));
                g_signal_connect( o, "selection-done",
				  G_CALLBACK( optionMenus[i].fn ),
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

	/* The last druid page is blank without this call. */
        gtk_widget_show_all( sxsld->sincelast_window );

        create_autoCreate_ledger( sxsld );
        create_created_ledger( sxsld );
        create_to_create_ledger( sxsld );

	gnc_restore_window_size(GCONF_SECTION, GTK_WINDOW(sxsld->sincelast_window));

	/* Do not call show_all here. Screws up the gtkuimanager code */
        gtk_widget_show( sxsld->sincelast_window );

        creation_errors = NULL;
        process_auto_create_list(sxsld->autoCreateList, sxsld, &creation_errors);
        if (g_list_length(creation_errors) > 0)
        {
                creation_errors_dialog(creation_errors);
                creation_errors_free(creation_errors);
        }

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
  gnc_save_window_size( GCONF_SECTION, GTK_WINDOW(sxsld->sincelast_window) );
}

static void
generate_instances(SchedXaction *sx,
                   GDate *end,
                   GDate *reminderEnd,
                   GList **instanceList,
                   GList **reminderList,
                   GList **deadList)
{
        GDate gd;
        toCreateInstance *tci;
        reminderTuple *rt;
        reminderInstanceTuple *rit;
        void *seqStateData;

        g_assert( g_date_valid(end) );
        g_assert( g_date_valid(reminderEnd) );

        g_date_clear(&gd, 1);

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
                tci->origState = SX_UNDEF;
                tci->state     = SX_TO_CREATE;
                tci->prevState = SX_UNDEF;
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
process_auto_create_list(GList *autoCreateList, sxSinceLastData *sxsld, GList **creation_errors)
{
        toCreateTuple *tct;
        toCreateInstance *tci;
        GList *instances;

        gnc_suspend_gui_refresh();

        for ( ; autoCreateList ; autoCreateList = autoCreateList->next ) {
                tct = (toCreateTuple*)autoCreateList->data;
                
                for ( instances = tct->instanceList;
                      instances;
                      instances = instances->next ) {
                        tci = (toCreateInstance*)instances->data;
                        sxsld->autoCreatedCount +=
                                sxsld_process_to_create_instance( sxsld, tci, creation_errors );
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
                        gboolean allVarsBound = FALSE;

                        tci = (toCreateInstance*)insts->data;
                
                        /* tct->{sx,date} are already filled in. */
                        if ( ! tci->varBindings ) {
                                tci->varBindings = g_hash_table_new( g_str_hash,
                                                                     g_str_equal );

                                sxsl_get_sx_vars( tci->parentTCT->sx,
                                                  tci->varBindings );
                        }

                        rowText[0] = g_new0( char, MAX_DATE_LENGTH+1 );
                        qof_print_gdate( rowText[0], MAX_DATE_LENGTH, tci->date );
                        

                        switch ( tci->state ) {
                        case SX_TO_CREATE:
                            allVarsBound = TRUE;
                            g_hash_table_foreach( tci->varBindings,
                                                  andequal_numerics_set,
                                                  &allVarsBound );
                            rowText[1] = ( allVarsBound
                                           ? _( "Ready to create" ) /* READY_TEXT */ 
                                           : _( "Needs values for variables" ) /* NEEDS_BINDINGS_TEXT */
                                    );
                            break;
                        case SX_IGNORE:
                            rowText[1] = _( "Ignored" ) /* IGNORE_TEXT */ ;
                            break;
                        case SX_POSTPONE:
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
                        qof_print_gdate( rowText[0], MAX_DATE_LENGTH, rit->occurDate );
                        rowText[1] = "";
                        rowText[2] = g_new0( gchar, 5 ); /* FIXME: appropriate size? */
                        sprintf( rowText[2], "%d",
                                 (g_date_get_julian(rit->occurDate)
                                  - g_date_get_julian(rit->endDate)) );

                        instNode = gtk_ctree_insert_node( ctree, sxNode, NULL,
                                                          rowText,
                                                          0, NULL, NULL, NULL, NULL,
                                                          TRUE, TRUE );
                        gtk_ctree_node_set_row_data( ctree,
                                                     instNode,
                                                     (gpointer)rit );
                        g_signal_handlers_block_by_func( G_OBJECT(ctree),
                                                         sxsld_remind_row_toggle,
                                                         sxsld ); 
                        if ( rit->isSelected ) {
                                gtk_ctree_select( ctree, instNode );
                        }
                        g_signal_handlers_unblock_by_func( G_OBJECT(ctree),
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
        rowtext[2] = g_strdup( _("Obsolete") );

        gtk_clist_freeze( cl );
        gtk_clist_clear( cl );
        g_signal_handlers_block_by_func( G_OBJECT(cl),
                                         sxsld_obsolete_row_toggle,
                                         sxsld );

        for ( row = 0; removeList;
              row++, removeList = removeList->next ) {
                tdt = (toDeleteTuple*)removeList->data;

                rowtext[0] = xaccSchedXactionGetName( tdt->sx );

                fs = xaccSchedXactionGetFreqSpec( tdt->sx );
                xaccFreqSpecGetFreqStr( fs, tmp_str );
                /* XXX are we leaking memory here, by not 
                 * freeing previous rrowtext[1] ?? */
                rowtext[1] = tmp_str->str;

                gtk_clist_insert( cl, row, rowtext );
                gtk_clist_set_row_data( cl, row, tdt );
                if ( tdt->isSelected ) {
                        gtk_clist_select_row( cl, row, 0 );
                }
        }
        g_signal_handlers_unblock_by_func( G_OBJECT(cl),
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
                tci->state       = SX_TO_CREATE;
                tci->prevState   = SX_UNDEF;
                tci->origState   = SX_UNDEF;
                tci->varBindings = NULL;
                tci->node        = NULL;
                tci->sxStateData = rit->sxStateData;
                
                tct->instanceList =
                        g_list_append( tct->instanceList, tci );
                
                /* special auto-create-opt processing; process it now. */
                if ( autoCreateOpt ) {
                        GList *creation_errors = NULL;
                        list = NULL;
                        list = g_list_append( list, tct );
                        process_auto_create_list( list, sxsld, &creation_errors );
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

		g_date_set_time_t( &end, time(NULL) );
                daysInAdvance = xaccSchedXactionGetAdvanceCreation( sx );
                g_date_add_days( &end, daysInAdvance );
                
                endPlusReminders = end;
                daysInAdvance = xaccSchedXactionGetAdvanceReminder(sx);
                g_date_add_days(&endPlusReminders, daysInAdvance);

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
                                tci->state       = SX_POSTPONE;
                                tci->prevState   = SX_POSTPONE;
                                tci->origState   = SX_POSTPONE;

                                instanceList = g_list_append( instanceList, tci );
                                tci = NULL;
                        }
                        
                }

                generate_instances(sx,
                                   &end,
                                   &endPlusReminders,
                                   &instanceList,
                                   &sxsld->reminderList,
                                   &sxsld->toRemoveList);

                if (instanceList == NULL)
                        continue;

                xaccSchedXactionGetAutoCreate(sx, &autocreateState, &notifyState);
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
                GList *creation_errors = NULL;
                process_auto_create_list( sxsld->autoCreateList, sxsld, &creation_errors );
                if (g_list_length(creation_errors) > 0)
                {
                        creation_errors_dialog(creation_errors);
                        creation_errors_free(creation_errors);
                }
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

        tci = (toCreateInstance*)g_object_get_data( G_OBJECT(e), "tci" );
        g_assert( tci == sxsld->curSelTCI );

        varName = (gchar*)g_object_get_data( G_OBJECT(e), "varName" );
        num = (gnc_numeric*)g_object_get_data( G_OBJECT(e), "numeric" );
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

        gnc_embedded_window_close_page(sxsld->ac_window, sxsld->ac_register);
        gtk_widget_destroy(GTK_WIDGET(sxsld->ac_window));
        sxsld->ac_window = NULL;
        sxsld->ac_register = NULL;
        sxsld->ac_ledger = NULL;

        gnc_embedded_window_close_page(sxsld->created_window,
                                       sxsld->created_register);
        gtk_widget_destroy(GTK_WIDGET(sxsld->created_window));
        sxsld->created_window = NULL;
        sxsld->created_register = NULL;
        sxsld->created_ledger = NULL;

        gnc_embedded_window_close_page(sxsld->to_create_window,
                                       sxsld->to_create_register);
        gtk_widget_destroy(GTK_WIDGET(sxsld->to_create_window));
        sxsld->to_create_window = NULL;
        sxsld->to_create_register = NULL;
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

static gint
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
        gnc_commodity *first_cmdty = NULL;
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
                          g_date_get_day( tci->date ),
                          g_date_get_month( tci->date ),
                          g_date_get_year( tci->date ) );

        /* the accounts and amounts are in the kvp_frames of the splits. */
        osList = xaccTransGetSplitList( t );
        sList = xaccTransGetSplitList( newT );
        if ( (osList == NULL) || (sList == NULL) ) {
                PERR( "\tseen transaction w/o splits. :(" );
                xaccTransDestroy( newT );
                xaccTransCommitEdit( newT );
                return 13;
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

        for ( ; sList && osList; sList = sList->next, osList = osList->next)
        {
                Account *acct;
                gnc_commodity *split_cmdty = NULL;

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
                        if (kvp_val == NULL) {
                                GString *err = g_string_new("");
                                g_string_printf(err, "Null account kvp value for SX [%s], cancelling creation.",
                                                xaccSchedXactionGetName(createUD->tci->parentTCT->sx));
                                *createUD->creation_errors = g_list_append(*createUD->creation_errors, err);
                                errFlag = TRUE;
                                break;
                        }
                        acct_guid = kvp_value_get_guid( kvp_val );
                        acct = xaccAccountLookup( acct_guid, gnc_get_current_book ());
                        if (acct == NULL)
                        {
                                const char *guidStr;
                                GString *err;
                                guidStr = guid_to_string((const GUID*)acct_guid);
                                err = g_string_new("");
                                g_string_printf(err, "Unknown account for guid [%s], cancelling SX [%s] creation.",
                                                guidStr, xaccSchedXactionGetName(createUD->tci->parentTCT->sx));
                                g_free((char*)guidStr);
                                *createUD->creation_errors = g_list_append(*createUD->creation_errors, err);
                                errFlag = TRUE;
                                break;
                        }

                        split_cmdty = xaccAccountGetCommodity(acct);
                        if (first_cmdty == NULL)
                        {
                                first_cmdty = split_cmdty;
                                xaccTransSetCurrency(newT, first_cmdty);
                        }

                        xaccAccountBeginEdit(acct);
                        xaccAccountInsertSplit(acct, split);
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
                        if (str != NULL && strlen(str) != 0) {
                                if (!gnc_exp_parser_parse_separate_vars(str, &credit_num,
                                                                        &parseErrorLoc,
                                                                        actualVars))
                                {
                                        GString *err = g_string_new("");
                                        g_string_printf(err, "Error parsing SX [%s] credit formula [%s] at [%s]: %s",
                                                        xaccSchedXactionGetName(createUD->tci->parentTCT->sx),
                                                        str, parseErrorLoc, gnc_exp_parser_error_string());
                                        *createUD->creation_errors = g_list_append(*createUD->creation_errors, err);
                                        credit_num = gnc_numeric_create( 0, 1 );
                                }
                        }
                        
                        kvp_val = kvp_frame_get_slot_path( split_kvpf,
                                                           GNC_SX_ID,
                                                           GNC_SX_DEBIT_FORMULA,
                                                           NULL);
                        str = kvp_value_get_string( kvp_val );

                        debit_num = gnc_numeric_create( 0, 1 );
                        if (str != NULL && strlen(str) != 0) {
                                if (!gnc_exp_parser_parse_separate_vars(str, &debit_num,
                                                                        &parseErrorLoc,
                                                                        actualVars))
                                {
                                        GString *err = g_string_new("");
                                        g_string_printf(err, "Error parsing SX [%s] debit formula [%s] at [%s]: %s",
                                                        xaccSchedXactionGetName(createUD->tci->parentTCT->sx),
                                                        str, parseErrorLoc, gnc_exp_parser_error_string());
                                        *createUD->creation_errors = g_list_append(*createUD->creation_errors, err);
                                        debit_num = gnc_numeric_create( 0, 1 );
                                }

                        }
                        
                        final = gnc_numeric_sub_fixed( debit_num, credit_num );
                        
                        gncn_error = gnc_numeric_check(final);
                        if (gncn_error != GNC_ERROR_OK) {
                                GString *err = g_string_new("");
                                g_string_printf(err, "Error %d in SX [%s] final gnc_numeric value, using 0 instead.", 
                                                gncn_error,
                                                xaccSchedXactionGetName(createUD->tci->parentTCT->sx));
                                *createUD->creation_errors = g_list_append(*createUD->creation_errors, err);
                                final = gnc_numeric_create(0, 1);
                        }

                        xaccSplitSetValue(split, final);
                        if (! gnc_commodity_equal(split_cmdty, first_cmdty))
                        {
                                GString *exchange_rate_var_name = g_string_sized_new(16);
                                gnc_numeric *exchange, amt;

                                /*
                                GNCPriceDB *price_db = gnc_pricedb_get_db(gnc_get_current_book());
                                GNCPrice *price;

                                price = gnc_pricedb_lookup_latest(price_db, first_cmdty, split_cmdty);
                                if (price == NULL)
                                {
                                        price = gnc_pricedb_lookup_latest(price_db, split_cmdty, first_cmdty);
                                        if (price == NULL)
                                        {
                                                GString *err = g_string_new("");
                                                g_string_printf(err, "could not find pricedb entry for commodity-pair (%s, %s).",
                                                                gnc_commodity_get_mnemonic(first_cmdty),
                                                                gnc_commodity_get_mnemonic(split_cmdty));
                                                exchange = gnc_numeric_create(1, 1);
                                                *createUD->creation_errors = g_list_append(*createUD->creation_errors, err);

                                        }
                                        else
                                        {
                                                exchange = gnc_numeric_div(gnc_numeric_create(1,1),
                                                                           gnc_price_get_value(price),
                                                                           1000, GNC_HOW_RND_ROUND);
                                        }
                                }
                                else
                                {
                                        exchange = gnc_price_get_value(price);
                                }
                                */

                                g_string_printf(exchange_rate_var_name, "%s -> %s",
                                                gnc_commodity_get_mnemonic(split_cmdty),
                                                gnc_commodity_get_mnemonic(first_cmdty));
                                exchange = (gnc_numeric*)g_hash_table_lookup(actualVars, exchange_rate_var_name->str);
                                if (exchange == NULL)
                                {
                                        exchange = g_new0(gnc_numeric, 1);
                                        *exchange = gnc_numeric_create(0, 1);
                                }
                                g_string_free(exchange_rate_var_name, TRUE);

                                amt = gnc_numeric_mul(final, *exchange, 1000, GNC_HOW_RND_ROUND);
                                xaccSplitSetAmount(split, amt);
                        }
                        xaccSplitScrub( split );
                }
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

        if (errFlag) {
                PERR("Some error in new transaction creation...");
                xaccTransDestroy(newT);
                xaccTransCommitEdit(newT);
                return 13;
        }

        {
                kvp_frame *txn_frame;
                /* set a kvp-frame element in the transaction indicating and
                 * pointing-to the SX this was created from. */
                txn_frame = xaccTransGetSlots(newT);
                kvp_frame_set_guid(txn_frame, "from-sched-xaction", 
                                   xaccSchedXactionGetGUID(tci->parentTCT->sx));
        }

        xaccTransCommitEdit(newT);

        if ( createUD->createdGUIDs != NULL ) {
                *createUD->createdGUIDs =
                        g_list_append( *(createUD->createdGUIDs),
                                       (gpointer)xaccTransGetGUID(newT) );
        }

        return 0;
}

/**
 * This should be called with the dates in increasing order, or the last call
 * will set the last occur date incorrectly.
 **/
static void
create_transactions_on(SchedXaction *sx,
                       GDate *gd,
                       toCreateInstance *tci,
                       GList **createdGUIDs,
                       GList **creation_errors)
{
        createData createUD;
        AccountGroup *ag;
        Account *acct;
        const char *id;

        if (tci) {
                g_assert(g_date_compare(gd, tci->date) == 0);
        }

        ag = gnc_book_get_template_group( gnc_get_current_book () );
        id = guid_to_string( xaccSchedXactionGetGUID(sx) );
        if ( !(ag && id) ) {
                return;
        }
        /* This looks strange but it's right.  The account is
           named after the guid string. */
        acct = xaccGetAccountFromName( ag, id );
        if (!acct) {
                return;
        }

        createUD.tci = tci;
        createUD.createdGUIDs = createdGUIDs;
        createUD.creation_errors = creation_errors;
        xaccAccountForEachTransaction(acct,
                                      create_each_transaction_helper,
                                      /*tct*/ &createUD);
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

static gint
_get_vars_helper(Transaction *txn, void *var_hash_data)
{
        GHashTable *var_hash = (GHashTable*)var_hash_data;
        GList *split_list;
        kvp_frame *kvpf;
        kvp_value *kvp_val;
        Split *s;
        char *str;
        gnc_commodity *first_cmdty = NULL;

        split_list = xaccTransGetSplitList(txn);

        if ( split_list == NULL ) {
                return 1;
        }

        for ( ; split_list ; split_list = split_list->next ) {
                gnc_commodity *split_cmdty = NULL;
                GUID *acct_guid;
                Account *acct;

                s = (Split*)split_list->data;
                kvpf = xaccSplitGetSlots(s);
                kvp_val = kvp_frame_get_slot_path(kvpf,
                                                  GNC_SX_ID,
                                                  GNC_SX_ACCOUNT,
                                                  NULL);
                acct_guid = kvp_value_get_guid( kvp_val );
                acct = xaccAccountLookup( acct_guid, gnc_get_current_book ());
                split_cmdty = xaccAccountGetCommodity(acct);
                if (first_cmdty == NULL)
                {
                        first_cmdty = split_cmdty;
                }
                
                if (! gnc_commodity_equal(split_cmdty, first_cmdty))
                {
                        gnc_numeric *tmp_num;
                        GString *var_name = g_string_sized_new(16);
                        g_string_printf(var_name, "%s -> %s",
                                        gnc_commodity_get_mnemonic(split_cmdty),
                                        gnc_commodity_get_mnemonic(first_cmdty));
                        tmp_num = g_new0(gnc_numeric, 1);
                        *tmp_num = gnc_numeric_create(0, 1);
                        g_hash_table_insert(var_hash, g_strdup(var_name->str), tmp_num);
                        g_string_free(var_name, TRUE);
                }

                // existing... ------------------------------------------
                
                kvp_val = kvp_frame_get_slot_path( kvpf,
                                                   GNC_SX_ID,
                                                   GNC_SX_CREDIT_FORMULA,
                                                   NULL);
                if ( kvp_val != NULL ) {
                        str = kvp_value_get_string( kvp_val );
                        if ( str && strlen(str) != 0 ) {
                                parse_vars_from_formula( str, var_hash, NULL );
                        }
                }

                kvp_val = kvp_frame_get_slot_path( kvpf,
                                                   GNC_SX_ID,
                                                   GNC_SX_DEBIT_FORMULA,
                                                   NULL);
                if ( kvp_val != NULL ) {
                        str = kvp_value_get_string( kvp_val );
                        if ( str && strlen(str) != 0 ) {
                                parse_vars_from_formula( str, var_hash, NULL );
                        }
                }
        }

        return 0;
}

void
sxsl_get_sx_vars( SchedXaction *sx, GHashTable *var_hash )
{
        AccountGroup *ag;
        Account *acct;
        const char *id;

        ag = gnc_book_get_template_group( gnc_get_current_book () );
        id = guid_to_string( xaccSchedXactionGetGUID(sx) );
        /* Get account named after guid string. */
        acct = xaccGetAccountFromName( ag, id );
        xaccAccountForEachTransaction(acct,
                                      _get_vars_helper,
                                      var_hash);

        g_hash_table_foreach( var_hash,
                              clear_variable_numerics,
                              (gpointer)var_hash );
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
        num = (gnc_numeric*)g_object_get_data( G_OBJECT(ent), "numeric" );
        str = g_string_new("");
        g_string_printf( str, "%0.2f", gnc_numeric_to_double( *num ) );
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
                g_string_printf( gstr, "%s: ", varName );
                label = gtk_label_new( gstr->str );
                gtk_label_set_justify( GTK_LABEL(label), GTK_JUSTIFY_RIGHT );
                g_string_free( gstr, TRUE );

                entry = gtk_entry_new();
                g_object_set_data( G_OBJECT(entry), "varName", varName );
                g_object_set_data( G_OBJECT(entry), "tci", tci );
                tmpNumValue = g_new0( gnc_numeric, 1 );
                *tmpNumValue = gnc_numeric_create( 0, 1 );
                g_object_set_data( G_OBJECT(entry), "numeric", tmpNumValue );
                if ( tableIdx == varHashSize ) {
                        /* Set a flag so we can know if we're the last row of
                         * the table. */
                        g_object_set_data( G_OBJECT(entry), "lastVisualElt",
					   (gpointer)1 );
                }

                gtk_widget_set_size_request( entry, 64, -1 );
                numValue = (gnc_numeric*)g_hash_table_lookup( tci->varBindings,
                                                              varName );
                if ( numValue != NULL ) {
                        numValueStr =
                                xaccPrintAmount( *numValue,
                                                 gnc_default_print_info( FALSE ) );
                        gtk_entry_set_text( GTK_ENTRY(entry), numValueStr );
                }

                /* fixme::2002.02.10 jsled testing */
                g_signal_connect( entry, "key-press-event",
				  G_CALLBACK( tct_table_entry_key_handle ),
				  NULL );
                g_signal_connect( entry, "changed",
				  G_CALLBACK( sxsincelast_entry_changed ),
				  sxsld );
                g_signal_connect( entry, "destroy",
				  G_CALLBACK(gnc_sxsld_free_entry_numeric),
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
               (gchar*)key, GPOINTER_TO_UINT(value),
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
                toRet = -1;
        }

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
                        case SX_TO_CREATE:
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
                        case SX_IGNORE:
                        case SX_POSTPONE:
                                /* We were {postpone,ignore} or to-create,
                                 * before, so either continue to ignore or
                                 * subtract out the txns. */
                                if ( tci->prevState != SX_TO_CREATE ) {
                                        continue;
                                }
                                toRet -= g_list_length( tci->createdTxnGUIDs );
                                break;
                        case SX_UNDEF:
                        case SX_MAX_STATE:
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
        //g_assert( sxsld->curSelTCI != NULL );
        g_return_if_fail(sxsld->curSelTCI != NULL);

        sxsld->curSelTCI->state = newState;

        newSensitivity = TRUE;
        newCtreeText = "FIXME";

        switch ( newState ) {
        case SX_TO_CREATE:
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
        case SX_IGNORE:
                newSensitivity = FALSE;
                newCtreeText = _( IGNORE_TEXT );
                break;
        case SX_POSTPONE:
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
        qof_print_gdate( tmpBuf, MAX_DATE_LENGTH, rit->occurDate );
        g_string_append_printf( msg, tmpBuf );
        g_string_append_printf( msg, "\n" );
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
                g_string_printf( userMsg,
                                 "You cannot skip instances of "
                                 "Scheduled Transactions. "
                                 "The following instances of \"%s\" "
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
        GtkWidget *vbox;
        Query *q;

        q = xaccMallocQuery();
        xaccQuerySetBook (q, gnc_get_current_book ());
        sxsld->ac_ledger = gnc_ledger_display_query( q,
                                                     GENERAL_LEDGER,
                                                     REG_STYLE_LEDGER );

	/* First the embedded window */
        vbox = glade_xml_get_widget( sxsld->gxml, AUTO_CREATE_VBOX );
	sxsld->ac_window =
	  gnc_embedded_window_new("SXWindowActions",
				     gnc_sxsld_menu_entries,
				     gnc_sxsld_menu_n_entries,
				     "gnc-sxed-window-ui.xml",
				     sxsld->sincelast_window,
				     FALSE, /* no accelerators */
				     sxsld);
	gtk_box_pack_start (GTK_BOX (vbox), GTK_WIDGET(sxsld->ac_window),
			    TRUE, TRUE, 0);

	/* Then the register in it */
	sxsld->ac_register = gnc_plugin_page_register_new_ledger(sxsld->ac_ledger);
	gnc_plugin_page_set_ui_description (sxsld->ac_register,
					    "gnc-plugin-page-sxregister-ui.xml");
	gnc_plugin_page_register_set_options (sxsld->ac_register,
					      NULL, NULL, 4, FALSE);
	gnc_embedded_window_open_page (sxsld->ac_window, sxsld->ac_register);

	/* Now configure the register */
        splitreg = gnc_ledger_display_get_split_register( sxsld->ac_ledger );
        gnc_split_register_config(splitreg,
                                  splitreg->type, splitreg->style,
                                  FALSE);
        gnc_split_register_show_present_divider( splitreg, FALSE );
}

static void
create_created_ledger( sxSinceLastData *sxsld )
{
        SplitRegister *splitreg;
        GtkWidget *vbox;
        Query *q;

        q = xaccMallocQuery();
        xaccQuerySetBook (q, gnc_get_current_book ());
        sxsld->created_ledger = gnc_ledger_display_query( q,
                                                          GENERAL_LEDGER,
                                                          REG_STYLE_LEDGER );

	/* First the embedded window */
        vbox = glade_xml_get_widget( sxsld->gxml, CREATED_VBOX );
	sxsld->created_window =
	  gnc_embedded_window_new("SXWindowActions",
				  gnc_sxsld_menu_entries,
				  gnc_sxsld_menu_n_entries,
				  "gnc-sxed-window-ui.xml",
				  sxsld->sincelast_window,
				  FALSE, /* no accelerators */
				  sxsld);
	gtk_box_pack_start (GTK_BOX (vbox), GTK_WIDGET(sxsld->created_window),
			    TRUE, TRUE, 0);

	/* Then the register in it */
	sxsld->created_register = gnc_plugin_page_register_new_ledger(sxsld->created_ledger);
	gnc_plugin_page_set_ui_description (sxsld->created_register,
					    "gnc-plugin-page-sxregister-ui.xml");
	gnc_plugin_page_register_set_options (sxsld->created_register,
					      NULL, NULL, 4, FALSE);
	gnc_embedded_window_open_page (sxsld->created_window, sxsld->created_register);

	/* Now configure the register */
        splitreg = gnc_ledger_display_get_split_register( sxsld->created_ledger );
        gnc_split_register_config(splitreg,
                                  splitreg->type, splitreg->style,
                                  FALSE);
        gnc_split_register_show_present_divider( splitreg, FALSE );
}

#if 0
static
void
sxsld_jump_to_real_txn( GtkAction *action, sxSinceLastData *sxsld )
{
        SplitRegister *reg;
	GNCSplitReg *gsr;
        Account *account;
        Account *leader;
        Split *split;

        reg = gnc_ledger_display_get_split_register(sxsld->to_create_ledger);
	gsr = gnc_ledger_display_get_user_data(sxsld->to_create_ledger);

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
		GncPluginPage *new_page;
                GNCSplitReg *gsr;

		new_page = gnc_plugin_page_register_new (account, FALSE);
		gnc_main_window_open_page (NULL, new_page);
		gsr = gnc_plugin_page_register_get_gsr (new_page);
		gnc_split_reg_jump_to_split(gsr, split);
        }
        
        g_signal_stop_emission_by_name(gsr, "jump");
}
#endif

static void
create_to_create_ledger( sxSinceLastData *sxsld )
{
        SplitRegister *splitreg;
        GtkWidget *vbox;
        Query *q;

        sxsld->to_create_ledger = gnc_ledger_display_template_gl( NULL );
        q = xaccMallocQuery();
        xaccQueryClear( q );
        gnc_ledger_display_set_query( sxsld->to_create_ledger, q );

 	/* First the embedded window */
	vbox = glade_xml_get_widget( sxsld->gxml, TO_CREATE_TXN_VBOX );
	sxsld->to_create_window =
	  gnc_embedded_window_new("SXWindowActions",
				  gnc_sxsld_menu_entries,
				  gnc_sxsld_menu_n_entries,
				  "gnc-sxed-to-create-window-ui.xml",
				  sxsld->sincelast_window,
				  FALSE, /* no accelerators */
				  sxsld);
	gtk_box_pack_start (GTK_BOX (vbox), GTK_WIDGET(sxsld->to_create_window),
			    TRUE, TRUE, 0);

	/* Then the register in it */
	sxsld->to_create_register = gnc_plugin_page_register_new_ledger(sxsld->to_create_ledger);
	gnc_plugin_page_set_ui_description (sxsld->to_create_register,
					    "gnc-sxed-to-create-window-ui.xml");
	gnc_plugin_page_register_set_options(sxsld->to_create_register,
                                             NULL, NULL, 4, TRUE);
	gnc_embedded_window_open_page (sxsld->to_create_window, sxsld->to_create_register);

	/* Now configure the register */
        splitreg = gnc_ledger_display_get_split_register( sxsld->to_create_ledger );
        gnc_split_register_config( splitreg, splitreg->type, splitreg->style,
                                   FALSE );
        gnc_split_register_show_present_divider( splitreg, FALSE );
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
        if ( tci->prevState      != SX_POSTPONE
             && tci->origState   != SX_POSTPONE
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
gnc_sxsld_free_entry_numeric( GObject *o, gpointer ud )
{
        gnc_numeric *num;
        num = (gnc_numeric*)g_object_get_data( o, "numeric" );
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

static
void
_adderror(gpointer data, gpointer user_data)
{
        GString *dialog_text = (GString*)user_data;
        g_string_append_printf(dialog_text, "- %s\n", ((GString*)data)->str);
}

static
void
creation_errors_dialog(GList *creation_errors)
{
        GString *dialog_text = g_string_new(_("The following errors were encountered while creating the Scheduled Transactions:\n"));
        g_list_foreach(creation_errors, (GFunc)_adderror, dialog_text);
        gnc_info_dialog(NULL, "%s", dialog_text->str);
        g_string_free(dialog_text, TRUE);
}

static void
_free_creation_errors(gpointer data, gpointer user_data_unused)
{
        g_string_free((GString*)data, TRUE);
}

static
void
creation_errors_free(GList *creation_errors)
{
        g_list_foreach(creation_errors, (GFunc)_free_creation_errors, NULL);
        g_list_free(creation_errors);
}
