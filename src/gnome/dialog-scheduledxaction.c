/********************************************************************\
 * dialog-scheduledxaction.c : dialog for scheduled transaction     *
 *    list and editor                                               *
 * Copyright (C) 2001,2002 Joshua Sled <jsled@asynchronous.org>     *
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
#include <locale.h>
#include <time.h>

#include "FreqSpec.h"
#include "SchedXaction.h"
#include "SX-book.h"
#include "SX-book-p.h"
#include "dialog-preferences.h"
#include "dialog-scheduledxaction.h"
#include "dialog-utils.h"
#include "gnc-book.h"
#include "gnc-component-manager.h"
#include "gnc-date.h"
#include "gnc-date-edit.h"
#include "gnc-dense-cal.h"
#include "gnc-embedded-window.h"
#include "gnc-engine.h"
#include "gnc-frequency.h"
#include "gnc-gconf-utils.h"
#include "gnc-gui-query.h"
#include "gnc-hooks.h"
#include "gnc-ledger-display.h"
#include "gnc-plugin-page.h"
#include "gnc-plugin-page-register.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "gnucash-sheet.h"

#include "gnc-split-reg.h"

/* FIXME: temp until variable-related-stuff settled. */
#include "dialog-sxsincelast.h"

#ifdef HAVE_LANGINFO_D_FMT
#include <langinfo.h>
#endif

static QofLogModule log_module = GNC_MOD_SX;

#define SX_LIST_GCONF_SECTION "dialogs/scheduled_trans/transaction_list"
#define SX_LIST_WIN_PREFIX "sx_list_win"
#define SX_LIST_GLADE_NAME "Scheduled Transaction List"
#define SX_LIST "sched_xact_list"
#define SX_LIST_UPCOMING_FRAME "upcoming_cal_frame"
#define SX_EDITOR_GLADE_NAME "Scheduled Transaction Editor"

#define SXED_WIN_PREFIX "sx_editor_win"
#define SXED_NAME_ENTRY "sxe_name"
#define SXED_LAST_OCCUR_LABEL "last_occur_label"
#define AUTOCREATE_OPT "autocreate_opt"
#define NOTIFY_OPT "notify_opt"
#define ADVANCE_OPT "advance_opt"
#define ADVANCE_DAYS_SPIN "advance_days"
#define REMIND_OPT "remind_opt"
#define REMIND_DAYS_SPIN "remind_days"
#define END_DATE_BOX "end_date_hbox"
#define END_ENTRY "end_nentry"
#define REMAIN_ENTRY "remain_nentry"

#define SX_GLADE_FILE "sched-xact.glade"

#define END_NEVER_OPTION 0
#define END_DATE_OPTION  1
#define NUM_OCCUR_OPTION 2

#define NUM_LEDGER_LINES_DEFAULT 6

#define EX_CAL_NUM_MONTHS 6
#define EX_CAL_MO_PER_COL 2

#define GNC_D_WIDTH 25
#define GNC_D_BUF_WIDTH 26

/** Datatypes ***********************************************************/

typedef enum _EndTypeEnum {
        END_NEVER,
        END_DATE,
        END_OCCUR,
} EndType;

/* Runtime/dialog information about a particular SX. */
typedef struct _SxRuntimeInfo
{
        SchedXaction *sx;
        // the gnc-dense-cal mark-tag
        gint         markTag;
        // which row in the GTK CList this SX is.
        gint         row;
} SxRuntimeInfo;

struct _SchedXactionDialog
{
        GtkWidget   *dialog;
        GladeXML    *gxml;
        GncDenseCal *gdcal;
        GHashTable  *sxData;

        gint        currentSortCol;
        GtkSortType currentSortType;
};

struct _SchedXactionEditorDialog
{
        GladeXML *gxml;
        GtkWidget *dialog;
        SchedXactionDialog *sxd;
        SchedXaction *sx;
        /* If this is a new scheduled transaction or not. */
        int newsxP;

        /* The various widgets in the dialog */
        GNCLedgerDisplay *ledger;
        GNCSplitReg *gsr;
        GnucashRegister *reg;

        GNCFrequency *gncfreq;
        GncDenseCal *example_cal;
        GDate **cal_marks;
        gint markId;

        GtkEditable *nameEntry;

        GtkLabel *lastOccurLabel;

        GtkToggleButton *autocreateOpt;
        GtkToggleButton *notifyOpt;
        GtkToggleButton *advanceOpt;
        GtkSpinButton *advanceSpin;
        GtkToggleButton *remindOpt;
        GtkSpinButton *remindSpin;

        GtkToggleButton *optEndDate;
        GtkToggleButton *optEndNone;
        GtkToggleButton *optEndCount;
        GtkEntry *endCountEntry;
        GtkEntry *endRemainEntry;
        GNCDateEdit *endDateEntry;

        char *sxGUIDstr;

        GncEmbeddedWindow *embed_window;
	GncPluginPage *plugin_page;
};

/** Prototypes **********************************************************/

static void putSchedXactionInDialog( gpointer data, gpointer user_data );

static void generate_instances( SchedXaction *sx,
                                GDate *end, GList **instanceList );

static void schedXact_populate( SchedXactionDialog * );
static void schedXact_editor_create_freq_sel( SchedXactionEditorDialog *sxed );
static void schedXact_editor_create_ledger( SchedXactionEditorDialog *sxed );
static void schedXact_editor_populate( SchedXactionEditorDialog * );

static void sxd_close_handler ( gpointer user_data );

static void new_button_clicked( GtkButton *b, gpointer d );
static void edit_button_clicked( GtkButton *b, gpointer d );
static void delete_button_clicked( GtkButton *b, gpointer d );
static void close_button_clicked( GtkButton *b, gpointer d );
static void gnc_sxl_record_size( SchedXactionDialog *sxd );
static void gnc_sxd_row_click_handler( GtkCList *clist,
                                       gint col,
                                       gpointer ud );
static void gnc_sxd_set_sort_compare( GtkCList *cl, gint col );
static gint gnc_sxd_clist_compare_sx_name( GtkCList *cl,
                                           gconstpointer a,
                                           gconstpointer b );
static gint gnc_sxd_clist_compare_sx_freq( GtkCList *cl,
                                           gconstpointer a,
                                           gconstpointer b );
static gint gnc_sxd_clist_compare_sx_next_occur( GtkCList *cl,
                                                 gconstpointer a,
                                                 gconstpointer b );

static void gnc_sxed_record_size( SchedXactionEditorDialog *sxed );
static void gnc_sxed_get_widgets( SchedXactionEditorDialog *sxed );
static void endgroup_rb_toggled( GtkButton *b, gpointer d );
static void set_endgroup_toggle_states( SchedXactionEditorDialog *sxed, EndType t );
static void advance_toggle( GtkButton *b, SchedXactionEditorDialog *sxed );
static gboolean gnc_sxed_check_consistent( SchedXactionEditorDialog *sxed );
static gboolean gnc_sxed_check_changed( SchedXactionEditorDialog *sxed );
static void free_keys_and_numerics_ea( gpointer key,
                                       gpointer value,
                                       gpointer user_data );
static void gnc_sxed_save_sx( SchedXactionEditorDialog *sxed );
static void gnc_sxed_freq_changed( GNCFrequency *gf, gpointer ud );
static void sxed_excal_update_adapt( GtkObject *o, gpointer ud );
static void gnc_sxed_update_cal( SchedXactionEditorDialog *sxed );

static void gnc_sxed_reg_check_close(SchedXactionEditorDialog *sxed);

static gint sxed_close_event( GtkDialog *dlg, gpointer ud );

static gboolean sxed_confirmed_cancel( SchedXactionEditorDialog *sxed );

static gboolean editor_component_sx_equality( gpointer find_data,
                                              gpointer user_data );

static SxRuntimeInfo* _new_sx_runtime_info( SchedXaction *sx );
static void _clear_runtime_info_row( gpointer key, gpointer value, gpointer user_data );


static void gnc_sxed_cmd_edit_cut (GtkAction *action, SchedXactionEditorDialog *sxed);
static void gnc_sxed_cmd_edit_copy (GtkAction *action, SchedXactionEditorDialog *sxed);
static void gnc_sxed_cmd_edit_paste (GtkAction *action, SchedXactionEditorDialog *sxed);

static GtkActionEntry gnc_sxed_menu_entries [] =
{
	/* Toplevel */
	{ "EditAction", NULL, N_("_Edit"), NULL, NULL, NULL },
	{ "ViewAction", NULL, N_("_View"), NULL, NULL, NULL },
	{ "ViewAction", NULL, N_("_View"), NULL, NULL, NULL },
	{ "ActionsAction", NULL, N_("_Actions"), NULL, NULL, NULL },
	{ "TransactionAction", NULL, N_("_Transaction"), NULL, NULL, NULL },

	/* Edit menu */
	{ "EditCutAction", GTK_STOCK_CUT, N_("Cu_t"), "<control>x",
	  NULL,
	  G_CALLBACK (gnc_sxed_cmd_edit_cut) },
	{ "EditCopyAction", GTK_STOCK_COPY, N_("_Copy"), "<control>c",
	  NULL,
	  G_CALLBACK (gnc_sxed_cmd_edit_copy) },
	{ "EditPasteAction", GTK_STOCK_PASTE, N_("_Paste"), "<control>v",
	  NULL,
	  G_CALLBACK (gnc_sxed_cmd_edit_paste) },

};
static guint gnc_sxed_menu_n_entries = G_N_ELEMENTS (gnc_sxed_menu_entries);

/** Implementations *****************************************************/

static
void
sxd_close_handler ( gpointer user_data )
{
        SchedXactionDialog        *sxd = user_data;
        
        gnc_sxl_record_size( sxd );
        gtk_widget_hide( sxd->dialog );
}

static
void
_clear_runtime_info_row( gpointer key, gpointer value, gpointer user_data )
{
        SxRuntimeInfo *sxri;
        sxri = (SxRuntimeInfo*)value;
        sxri->row = -1;
}

void
gnc_sxd_list_refresh( SchedXactionDialog *sxd )
{
        GList *sxList;
        GtkCList *cl;

        /* Update the clist. */
        cl = GTK_CLIST( glade_xml_get_widget( sxd->gxml, SX_LIST ) );
        gtk_clist_freeze( cl );

        gtk_clist_clear( cl );
        // Also, flush the row-numbers from storage
        g_hash_table_foreach( sxd->sxData, _clear_runtime_info_row, NULL );
        sxList = gnc_book_get_schedxactions( gnc_get_current_book() );
        g_list_foreach( sxList, putSchedXactionInDialog, sxd );

        gtk_clist_thaw( cl );
}

static
void
sxed_close_handler ( gpointer user_data )
{
        SchedXactionEditorDialog *sxed = user_data;

        gnc_sxed_reg_check_close( sxed );
        gnc_sxed_record_size( sxed );
        gtk_widget_destroy( sxed->dialog );
        /* The data will be cleaned up in the destroy handler. */
}

static
void
close_button_clicked( GtkButton *b, gpointer d )
{
        sxd_close_handler( d );
}

/**
 * @return TRUE if the user does want to cancel, FALSE if not.  If TRUE is
 * returned, the register's changes have been cancelled.
 **/
static
gboolean
sxed_confirmed_cancel( SchedXactionEditorDialog *sxed )
{
        SplitRegister *reg;

        reg = gnc_ledger_display_get_split_register( sxed->ledger );
        /* check for changes */
        if ( gnc_sxed_check_changed( sxed ) ) {
                const char *sx_changed_msg =
                        _( "This SX has changed; are you "
                           "sure you want to cancel?" );
                if (!gnc_verify_dialog(sxed->dialog, FALSE, sx_changed_msg)) {
                        return FALSE;
                }
        }
        /* cancel ledger changes */
        gnc_split_register_cancel_cursor_trans_changes( reg );
        return TRUE;
}

static
void
editor_cancel_button_clicked( GtkButton *b, SchedXactionEditorDialog *sxed )
{
        /* close */
        gnc_close_gui_component_by_data( DIALOG_SCHEDXACTION_EDITOR_CM_CLASS,
                                         sxed );
}

static
void
editor_help_button_clicked(GtkButton *b, SchedXactionEditorDialog *sxed)
{
	gnc_gnome_help(HF_USAGE, HL_SXEDITOR);
}

static void
set_var_to_random_value( gpointer key, gpointer value, gpointer ud )
{
        if ( !value ) {
                value = g_new0( gnc_numeric, 1 );
        }
        *(gnc_numeric*)value =
                double_to_gnc_numeric( rand() + 2, 1,
                                       GNC_NUMERIC_RND_MASK
                                       | GNC_RND_FLOOR );
        g_hash_table_insert( ud, key, value );
}

static
void
free_keys_and_numerics_ea( gpointer key, gpointer val, gpointer ud )
{
        g_assert( key );
        g_assert( val );
        g_free( (gchar*)key );
        g_free( (gnc_numeric*)val );
}

static
void
editor_ok_button_clicked( GtkButton *b, SchedXactionEditorDialog *sxed )
{
        GNCBook *book;
        GList *sxList;

        if ( !gnc_sxed_check_consistent( sxed ) ) 
                return;

        gnc_sxed_save_sx( sxed );

        /* add to list */
        if ( sxed->newsxP ) {
                book = gnc_get_current_book ();
                sxList = gnc_book_get_schedxactions( book );
                sxList = g_list_append( sxList, sxed->sx );
                gnc_book_set_schedxactions( book, sxList );
                sxed->newsxP = FALSE;
        }
        // regardless, do this so the close handler won't complain.
        sxed->sx = NULL;

        /* update lists */
        /* We now do this by getting the list of SX Lists and updating them
           [if they exist].  Otherwise, our pointer to our SXD might not be
           valid; see Bug#103629. */
        {
                GList *listDialogs, *ldIter;
                listDialogs =
                        gnc_find_gui_components( DIALOG_SCHEDXACTION_CM_CLASS,
                                                 NULL, NULL );
                for ( ldIter = listDialogs;
                      ldIter != NULL;
                      ldIter = ldIter->next )
                {
                        gnc_sxd_list_refresh( (SchedXactionDialog*)ldIter
                                              ->data );
                }
                if ( listDialogs != NULL )
                {
                        g_list_free( listDialogs );
                }
        }

        /* cleanup */
        gnc_close_gui_component_by_data( DIALOG_SCHEDXACTION_EDITOR_CM_CLASS,
                                         sxed );
}

/**
 * Checks to see if the SX has been modified from it's previously-saved
 * state.
 * @return TRUE if this is a 'new' SX, or if the SX has changed from it's
 *   previous configuration.
 **/
static
gboolean
gnc_sxed_check_changed( SchedXactionEditorDialog *sxed )
{
        if ( sxed->newsxP )
                return TRUE;

        /* name */
        {
                char *name;

                name = gtk_editable_get_chars( GTK_EDITABLE(sxed->nameEntry), 0, -1 );
                if ( strlen(name) == 0 ) {
                        return TRUE;
                        
                }
                if ( (xaccSchedXactionGetName(sxed->sx) == NULL)
                     || (strcmp( xaccSchedXactionGetName(sxed->sx),
                                 name ) != 0) ) {
                        return TRUE;
                }
        }

        /* end options */
        {
                /* dialog says... no end */
                if ( gtk_toggle_button_get_active( sxed->optEndNone ) ) {
                        if ( xaccSchedXactionHasEndDate(sxed->sx)
                             || xaccSchedXactionHasOccurDef(sxed->sx) ) {
                                return TRUE;
                        }
                }

                /* dialog says... end date */
                if ( gtk_toggle_button_get_active( sxed->optEndDate ) ) {
                        GDate sxEndDate, dlgEndDate;

                        if ( ! xaccSchedXactionHasEndDate( sxed->sx ) ) {
                                return TRUE;
                        }
                        sxEndDate = *xaccSchedXactionGetEndDate( sxed->sx );
                        g_date_set_time( &dlgEndDate,
                                         gnc_date_edit_get_date( sxed->
                                                                 endDateEntry ) );

                        if ( g_date_compare( &sxEndDate, &dlgEndDate ) != 0 ) {
                                return TRUE;
                        }
                }

                /* dialog says... num occur */
                if ( gtk_toggle_button_get_active( sxed->optEndCount ) ) {
                        gint sxNumOccur, sxNumRem, dlgNumOccur, dlgNumRem;
			const gchar *text;

                        if ( ! xaccSchedXactionGetNumOccur( sxed->sx ) ) {
                                return TRUE;
                        }

			text = gtk_entry_get_text (sxed->endCountEntry);
			sscanf (text, "%d", &dlgNumOccur);

			text = gtk_entry_get_text (sxed->endRemainEntry);
			sscanf (text, "%d", &dlgNumRem);

                        sxNumOccur = xaccSchedXactionGetNumOccur( sxed->sx );
                        sxNumRem = xaccSchedXactionGetRemOccur( sxed->sx );

                        if ( (dlgNumOccur != sxNumOccur)
                             || (dlgNumRem != sxNumRem) ) {
                                return TRUE;
                        }
                }
        }

        /* SX options [autocreate, notify, reminder, advance] */
        {
                gboolean dlgAutoCreate, dlgNotify, sxAutoCreate, sxNotify;
                gint dlgAdvance, sxAdvance;
                gint dlgRemind, sxRemind;

                dlgAutoCreate =
                        gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON(sxed->
                                                                        autocreateOpt) );
                dlgNotify =
                        gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON(sxed->
                                                                        notifyOpt) );

                xaccSchedXactionGetAutoCreate( sxed->sx, &sxAutoCreate, &sxNotify );
                if ( ! ((dlgAutoCreate == sxAutoCreate)
                        && (dlgNotify == sxNotify)) ) {
                        return TRUE;
                }

                dlgAdvance = 0;
                if ( gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON(sxed->advanceOpt) ) ) {
                        dlgAdvance =
                                gtk_spin_button_get_value_as_int( GTK_SPIN_BUTTON(sxed->
                                                                                  advanceSpin) );
                }
                sxAdvance = xaccSchedXactionGetAdvanceCreation( sxed->sx );
                if ( dlgAdvance != sxAdvance ) {
                        return TRUE;
                }

                dlgRemind = 0;
                if ( gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON(sxed->remindOpt) ) ) {
                        dlgRemind =
                                gtk_spin_button_get_value_as_int( GTK_SPIN_BUTTON(sxed->remindSpin) );
                }
                sxRemind = xaccSchedXactionGetAdvanceReminder( sxed->sx );
                if ( dlgRemind != sxRemind ) {
                        return TRUE;
                }
        }

        /* FS, startdate */
        {
                FreqSpec *dlgFS, *sxFS;
                GDate dlgStartDate, sxStartDate;
                GString *dlgFSstr, *sxFSstr;
                gboolean fsStrCmpResult;

                dlgFS = xaccFreqSpecMalloc( gnc_get_current_book() );
                /* save gncFreq data */
                gnc_frequency_save_state( sxed->gncfreq, dlgFS, &dlgStartDate );
                dlgFSstr = g_string_sized_new( 16 );
                xaccFreqSpecGetFreqStr( dlgFS, dlgFSstr );
                /* get SX startdate/fs data */
                sxStartDate = *xaccSchedXactionGetStartDate( sxed->sx );
                sxFS = xaccSchedXactionGetFreqSpec( sxed->sx );
                sxFSstr = g_string_sized_new( 16 );
                xaccFreqSpecGetFreqStr( sxFS, sxFSstr );
                /* compare */

                fsStrCmpResult = /* lame version of comparison */
                        (strcmp( dlgFSstr->str, sxFSstr->str) != 0);
                g_string_free( dlgFSstr, TRUE );
                g_string_free( sxFSstr, TRUE );
                xaccFreqSpecFree( dlgFS );

                if ( (g_date_compare(&dlgStartDate, &sxStartDate) != 0)
                     ||  fsStrCmpResult ) {
                        return TRUE;
                }
        }

        /* template transactions */
        {
                SplitRegister *sr =
                        gnc_ledger_display_get_split_register( sxed->ledger );

                if ( gnc_split_register_changed( sr ) ) {
                        return TRUE;
                }
        }
        return FALSE;
}


/**
 * Holds the credit- and debit-sum for a given Transaction, as used in
 * gnc_sxed_check_consistent.
 **/
typedef struct _txnCreditDebitSums {
  gnc_numeric creditSum;
  gnc_numeric debitSum;
} txnCreditDebitSums;

static
void
set_sums_to_zero( gpointer key,
                  gpointer val,
                  gpointer ud )
{
  txnCreditDebitSums *tcds = (txnCreditDebitSums*)val;
  tcds->creditSum = gnc_numeric_zero();
  tcds->debitSum  = gnc_numeric_zero();
}

static
void
free_sums( gpointer key,
           gpointer val,
           gpointer ud )
{
  txnCreditDebitSums *tcds = (txnCreditDebitSums*)val;
  g_free( tcds );
}

static
void
check_credit_debit_balance( gpointer key,
                            gpointer val,
                            gpointer ud )
{
        txnCreditDebitSums *tcds = (txnCreditDebitSums*)val;
        gboolean *unbalanced = (gboolean*)ud;
        *unbalanced |= !(gnc_numeric_zero_p(
                                 gnc_numeric_sub_fixed( tcds->debitSum,
                                                        tcds->creditSum ) ));
#if GNC_DEBUG

        if ( gnc_numeric_zero_p( gnc_numeric_sub_fixed( tcds->debitSum,
                                                        tcds->creditSum ) ) ) {
                DEBUG( "%.8x | true [%s - %s = %s]",
                        (unsigned int)key,
                        gnc_numeric_to_string( tcds->debitSum ),
                        gnc_numeric_to_string( tcds->creditSum ),
                        gnc_numeric_to_string(gnc_numeric_sub_fixed( tcds->debitSum,
                                                                     tcds->creditSum )) );
        } else {
                DEBUG( "%.8x | false [%s - %s = %s]",
                        (unsigned int)key,
                        gnc_numeric_to_string( tcds->debitSum ),
                        gnc_numeric_to_string( tcds->creditSum ),
                        gnc_numeric_to_string(gnc_numeric_sub_fixed( tcds->debitSum,
                                                                     tcds->creditSum )) );
        }
#endif /* GNC_DEBUG */
}

/**
 * Checks to make sure that the SX is in a reasonable state to save.
 * @return true if checks out okay, false otherwise.
 **/
static
gboolean
gnc_sxed_check_consistent( SchedXactionEditorDialog *sxed )
{
        gint ttVarCount, splitCount;
        FreqSpec *fs;

        /* Do checks on validity and such, interrupting the user if
         * things aren't right.
         *
         * Features...
         * X support formulas [?!]
         * X balancing the SX if contain numeric-only formula data.
         *   X agreement with create-automagically/notification controls
         * X the 'will ever be valid' check should take num-occur vals into
         *   account.
         * X SX name is unique
         * X SX has a name
         * X "weekly" FS has some days set.
         * X "once" with reasonable start/end dates.
         *   X This doesn't work at the time the 'weekly' one was fixed with
         *     user-confirmation, below; the once SX is always valid.
         * [X more generically, creating a "not scheduled" SX is probably not
         *   right... ]
         */

        ttVarCount = 0;
        splitCount = 0;
        {
                static const int NUM_ITERS_WITH_VARS = 5;
                static const int NUM_ITERS_NO_VARS = 1;
                int numIters, i;
                GHashTable *vars, *txns;
                GList *splitList = NULL;
                char *str;
                kvp_frame *f;
                kvp_value *v;
                Split *s;
                Transaction *t;
                gnc_numeric tmp;
                gboolean unbalanceable;
                gpointer unusedKey, unusedValue;

                unbalanceable = FALSE; /* innocent until proven guilty */
                vars = g_hash_table_new( g_str_hash, g_str_equal );
                txns = g_hash_table_new( g_direct_hash, g_direct_equal );
                numIters = NUM_ITERS_NO_VARS;
                /**
                 * Plan:
                 * . Do a first pass to get the variables.
                 * . Set each variable to random values.
                 * . see if we balance after that
                 *   . true: all good
                 *   . false: indicate to user, allow decision.
                 */

                /* FIXME: This _really_ shouldn't require a modification of the
                 * SX just to get the var names... */
                gnc_split_register_save ( gnc_ledger_display_get_split_register(sxed->ledger),
                                          FALSE );
                /* numeric-formulas-get-balanced determination */
                sxsl_get_sx_vars( sxed->sx, vars );

                ttVarCount = g_hash_table_size( vars );
                if ( ttVarCount != 0 ) {
                        /* balance with random variable bindings some number
                         * of times in an attempt to ferret out
                         * un-balanceable transactions.
                         * 
                         * NOTE: The Real Way to do this is with some
                         * symbolic math to eliminate the variables.  This is
                         * hard, and we don't do it.  This solution will
                         * suffice for now, and perhaps for the lifetime of
                         * the software. --jsled */
                        numIters = NUM_ITERS_WITH_VARS;
                }

                srand(time(NULL));
                for ( i=0; i < numIters && !unbalanceable; i++ ) {
                        g_hash_table_foreach( vars, set_var_to_random_value,
                                              (gpointer)vars );
                        g_hash_table_foreach( txns, set_sums_to_zero, NULL );
                        tmp = gnc_numeric_zero();

                        splitList = xaccSchedXactionGetSplits( sxed->sx );
                        splitCount += g_list_length( splitList );

                        for ( ; splitList; splitList = splitList->next )
                        {
                                txnCreditDebitSums *tcds;
                                s = (Split*)splitList->data;
                                t = xaccSplitGetParent( s );

                                if ( !(tcds =
                                       (txnCreditDebitSums*)g_hash_table_lookup( txns,
                                                                                 (gpointer)t )) )
                                {
                                        tcds = g_new0( txnCreditDebitSums, 1 );
                                        tcds->creditSum = gnc_numeric_zero();
                                        tcds->debitSum  = gnc_numeric_zero();
                                        g_hash_table_insert( txns, (gpointer)t, (gpointer)tcds );
                                }

                                f = xaccSplitGetSlots( s );
                                v = kvp_frame_get_slot_path( f,
                                                             GNC_SX_ID,
                                                             GNC_SX_CREDIT_FORMULA,
                                                             NULL );
                                if ( v
                                     && (str = kvp_value_get_string(v))
                                     && strlen( str ) != 0 ) {
                                        if ( parse_vars_from_formula( str, vars, &tmp ) < 0 ) {
                                                GString *errStr;

                                                errStr = g_string_sized_new( 32 );
                                                g_string_printf( errStr,
                                                                 _( "Couldn't parse credit formula for "
                                                                    "split \"%s\"." ),
                                                                 xaccSplitGetMemo( s ) );
                                                gnc_error_dialog( GTK_WIDGET(sxed->dialog),
                                                                  errStr->str );
                                                g_string_free( errStr, TRUE );

                                                return FALSE;
                                        }
                                        tcds->creditSum =
                                                gnc_numeric_add( tcds->creditSum, tmp, 100,
                                                                 (GNC_DENOM_AUTO | GNC_DENOM_LCD) );
                                        tmp = gnc_numeric_zero();
                                }
                                v = kvp_frame_get_slot_path( f,
                                                             GNC_SX_ID,
                                                             GNC_SX_DEBIT_FORMULA,
                                                             NULL );
                                if ( v
                                     && (str = kvp_value_get_string(v))
                                     && strlen(str) != 0 ) {
                                        if ( parse_vars_from_formula( str, vars, &tmp ) < 0 ) {
                                                GString *errStr;

                                                errStr = g_string_sized_new( 32 );
                                                g_string_printf( errStr,
                                                                 _( "Couldn't parse debit formula for "
                                                                    "split \"%s\"." ),
                                                                 xaccSplitGetMemo( s ) );
                                                gnc_error_dialog( GTK_WIDGET(sxed->dialog),
                                                                  (gchar*)errStr->str );
                                                g_string_free( errStr, TRUE );

                                                return FALSE;
                                        }
                                        tcds->debitSum = gnc_numeric_add( tcds->debitSum, tmp, 100,
                                                                          (GNC_DENOM_AUTO | GNC_DENOM_LCD) );
                                        tmp = gnc_numeric_zero();
                                }
                        }

                        g_hash_table_foreach( txns,
                                              check_credit_debit_balance,
                                              &unbalanceable );
                }

                /* Subtract out pre-defined vars */
                if ( g_hash_table_lookup_extended( vars, "i",
                                                   &unusedKey,
                                                   &unusedValue ) ) {
                        ttVarCount -= 1;
                }

                g_hash_table_foreach( vars,
                                      free_keys_and_numerics_ea,
                                      NULL );
                g_hash_table_destroy( vars );

                g_hash_table_foreach( txns, free_sums, NULL );
                g_hash_table_destroy( txns );

                if ( unbalanceable
                     && !gnc_verify_dialog( sxed->dialog, FALSE,
					    "%s",
					    _("The Scheduled Transaction Editor "
					      "cannot automatically\nbalance "
					      "this transaction. "
					      "Should it still be "
					      "entered?") ) ) {
                        return FALSE;
                }
        }

        /* read out data back into SchedXaction object. */
        /* FIXME: this is getting too deep; split out. */
        {
                gchar *name, *nameKey;
                gboolean nameExists, nameHasChanged;
                GList *sxList;

                name = gtk_editable_get_chars( GTK_EDITABLE(sxed->nameEntry), 0, -1 );
                if ( strlen(name) == 0 ) {
                        const char *sx_has_no_name_msg =
                                _( "Please name the Scheduled Transaction." );
                        gnc_error_dialog( sxed->dialog, sx_has_no_name_msg );
                        g_free( name );
                        return FALSE;
                        
                }

                nameExists = FALSE;
                nameKey = g_utf8_collate_key(name, -1);
                nameHasChanged =
                        (xaccSchedXactionGetName(sxed->sx) == NULL)
                        || (strcmp( xaccSchedXactionGetName(sxed->sx), name ) != 0);
                for ( sxList =
                          gnc_book_get_schedxactions( gnc_get_current_book() );
                      nameHasChanged && !nameExists && sxList ;
                      sxList = sxList->next ) {
                        char *existingName, *existingNameKey;
                        existingName =
                                xaccSchedXactionGetName( (SchedXaction*)sxList->
                                                         data );
                        existingNameKey = g_utf8_collate_key(existingName, -1);
                        nameExists |= ( strcmp(nameKey, existingNameKey) == 0 );
                        g_free( existingNameKey );
                }
                if ( nameHasChanged && nameExists ) {
                        const char *sx_has_existing_name_msg =
                                _( "A Scheduled Transaction with the "
                                   "name \"%s\" already exists.\n"
                                   "Are you sure you want to name "
                                   "this one the same?" );
                        if ( ! gnc_verify_dialog( sxed->dialog, FALSE,
						  sx_has_existing_name_msg,
						  name) ) {
                                g_free( nameKey );
                                g_free( name );
                                return FALSE;
                        }
                }
                g_free( nameKey );
                g_free( name );
        }

        {
                gboolean autocreateState, notifyState;

                autocreateState =
                        gtk_toggle_button_get_active(
                                GTK_TOGGLE_BUTTON(sxed->autocreateOpt) );
                notifyState =
                        gtk_toggle_button_get_active(
                                GTK_TOGGLE_BUTTON(sxed->notifyOpt) );

                if ( (ttVarCount > 0) && autocreateState ) {
                        gnc_warning_dialog( sxed->dialog,
					    _("Scheduled Transactions with variables\n"
					      "cannot be automatically created.") );
                        return FALSE;
                }

                /* Fix for part of Bug#121740 -- auto-create transactions are
                 * only valid if there's actually a transaction to create. */
                if ( autocreateState && splitCount == 0 ) {
                        gnc_warning_dialog( sxed->dialog,
                                            _("Scheduled Transactions without a template\n"
                                              "transaction cannot be automatically created.") );
                        return FALSE;
                }
        }

        /* deal with time. */
        {
                GDate startDate, endDate, nextDate;

                if ( !gtk_toggle_button_get_active(sxed->optEndDate)
                     && !gtk_toggle_button_get_active(sxed->optEndCount)
                     && !gtk_toggle_button_get_active(sxed->optEndNone) ) {
                        const char *sx_end_spec_msg =
                                _( "Please provide a valid end selection." );
                        gnc_error_dialog( sxed->dialog, sx_end_spec_msg );
                        return FALSE;
                }

                if ( gtk_toggle_button_get_active(sxed->optEndCount)) {
                        gint occur, rem;
			const gchar *text;

			text = gtk_entry_get_text (sxed->endCountEntry);
			sscanf (text, "%d", &occur);

			text = gtk_entry_get_text (sxed->endRemainEntry);
			sscanf (text, "%d", &rem);

                        if ( occur == 0 ) {
                                const char *sx_occur_count_zero_msg =
                                        _( "There must be some number of occurrences." );
                                gnc_error_dialog( sxed->dialog,
						  sx_occur_count_zero_msg );
                                return FALSE;
                        }

                        if ( rem > occur ) {
                                const char *sx_occur_counts_wrong_msg =
                                        _( "The number of remaining occurrences "
                                           "(%d) is greater than the number of "
                                           "total occurrences (%d)." );
                                gnc_error_dialog( sxed->dialog,
						  sx_occur_counts_wrong_msg,
						  rem, occur );
                                return FALSE;
                        }

                }

                g_date_clear( &endDate, 1 );
                if ( gtk_toggle_button_get_active(sxed->optEndDate) ) {
                        g_date_set_time( &endDate,
                                         gnc_date_edit_get_date( sxed->
                                                                 endDateEntry ) );
                }

                /* Now, see if the user is attempting to create a SX that can't exist
                 * [will never run]. */

                /* get the frequency spec data */
                fs = xaccFreqSpecMalloc( gnc_get_current_book() );
                gnc_frequency_save_state( sxed->gncfreq, fs, &startDate );
                /* Replicate just a smidgen of the code in the SX
                 * ...GetNextInstance routine */
                g_date_subtract_days( &startDate, 1 );
                xaccFreqSpecGetNextInstance( fs, &startDate, &nextDate );
                xaccFreqSpecFree( fs );

                if ( !g_date_valid( &nextDate )
                     || (g_date_valid( &endDate )
                         && (g_date_compare( &nextDate, &endDate ) > 0)) ) {
                        const char *invalid_sx_check_msg =
                                _( "You have attempted to create a Scheduled "
                                   "Transaction which will never run.\nDo you "
                                   "really want to do this?" );
                        if ( ! gnc_verify_dialog( sxed->dialog, FALSE,
						  invalid_sx_check_msg) ) {
                        
                                return FALSE;
                        }
                }
        }
        return TRUE;
}

/**
 * Saves the contents of the SX.  This assumes that gnc_sxed_check_consistent
 * has returned true.
 **/
static
void
gnc_sxed_save_sx( SchedXactionEditorDialog *sxed )
{
        /* name */
        {
                char *name;

                name = gtk_editable_get_chars( sxed->nameEntry, 0, -1 );
                xaccSchedXactionSetName( sxed->sx, name );
                g_free( name );
        }

        /* date */
        {
                GDate gdate;

                if ( gtk_toggle_button_get_active(sxed->optEndDate) ) {
                        /* get the end date data */
                        g_date_set_time( &gdate,
                                         gnc_date_edit_get_date(
                                                 sxed->endDateEntry ) );
                        xaccSchedXactionSetEndDate( sxed->sx, &gdate );
                        /* set the num occurances data */
                        xaccSchedXactionSetNumOccur( sxed->sx, 0 );
                } else if ( gtk_toggle_button_get_active(sxed->optEndCount) ) {
                        gint num;
			const gchar *text;

                        /* get the occurances data */
			text = gtk_entry_get_text (sxed->endCountEntry);
			sscanf (text, "%d", &num);
                        xaccSchedXactionSetNumOccur( sxed->sx, num );

			text = gtk_entry_get_text (sxed->endRemainEntry);
			sscanf (text, "%d", &num);
                        xaccSchedXactionSetRemOccur( sxed->sx, num );

                        g_date_clear( &gdate, 1 );
                        xaccSchedXactionSetEndDate( sxed->sx, &gdate );
                } else if ( gtk_toggle_button_get_active( sxed->optEndNone ) ) {
                        xaccSchedXactionSetNumOccur( sxed->sx, 0 );
                        g_date_clear( &gdate, 1 );
                        xaccSchedXactionSetEndDate( sxed->sx, &gdate );
                } else {
                        PERR( "No valid end specified\n" );
                }
        }

        /* Auto-create/notification states */
        {
                gboolean autocreateState, notifyState;

                autocreateState = gtk_toggle_button_get_active( sxed->autocreateOpt );
                notifyState = gtk_toggle_button_get_active( sxed->notifyOpt );
                /* "Notify" only makes sense if AutoCreate is actived;
                 * enforce that here. */
                xaccSchedXactionSetAutoCreate( sxed->sx,
                                               autocreateState,
                                               (autocreateState & notifyState) );
        }

        /* days in advance */
        {
                int daysInAdvance;

                daysInAdvance = 0;
                if ( gtk_toggle_button_get_active( sxed->advanceOpt ) ) {
                        daysInAdvance =
                                gtk_spin_button_get_value_as_int( sxed->advanceSpin );
                }
                xaccSchedXactionSetAdvanceCreation( sxed->sx, daysInAdvance );

                daysInAdvance = 0;
                if ( gtk_toggle_button_get_active( sxed->remindOpt ) ) {
                        daysInAdvance =
                                gtk_spin_button_get_value_as_int( sxed->remindSpin );
                }
                xaccSchedXactionSetAdvanceReminder( sxed->sx, daysInAdvance );
        }

        /* start date and freq spec */
        {
                FreqSpec *fs;
                GDate gdate;
		GString *str;

                fs = xaccSchedXactionGetFreqSpec( sxed->sx );
                gnc_frequency_save_state( sxed->gncfreq, fs, &gdate );

                str = g_string_new( "" );
                xaccFreqSpecGetFreqStr( fs, str );
                DEBUG( "fs: %s", str->str );

                /* now that we have it, set the start date */
                xaccSchedXactionSetStartDate( sxed->sx, &gdate );
        }

}

static void
autocreate_toggled( GtkObject *o, SchedXactionEditorDialog *sxed )
{
        if ( !gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(o)) ) {
                gtk_toggle_button_set_active( sxed->notifyOpt, FALSE );
        }
        gtk_widget_set_sensitive( GTK_WIDGET(sxed->notifyOpt),
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
        /* FIXME: this doesn't do what we want... :( */
        gtk_editable_set_editable( GTK_EDITABLE(spin), TRUE );
}

/* Local destruction of dialog */
static void
scheduledxaction_dialog_destroy(GtkObject *object, gpointer data)
{
        SchedXactionDialog *sxd = data;

        if ( !sxd ) return;

        gnc_unregister_gui_component_by_data
                (DIALOG_SCHEDXACTION_CM_CLASS, sxd);

        // FIXME: um.  We should free memory and stuff, here.

        g_free( sxd );
}

/* Local destruction of dialog */
static void
scheduledxaction_editor_dialog_destroy(GtkObject *object, gpointer data)
{
        int i;
        SchedXactionEditorDialog *sxed = data;

        if (sxed == NULL)
                return;

        gnc_unregister_gui_component_by_data
                (DIALOG_SCHEDXACTION_EDITOR_CM_CLASS, sxed);

        if ( sxed->newsxP ) {
                /* FIXME: WTF??? */
                xaccSchedXactionFree( sxed->sx );
        }
        sxed->sx = NULL;

        /* We don't need to deal with the ledger, as the gncRegWidget will do
         * so for us. [Is this still true?? -- jsled] */
        gnc_ledger_display_close( sxed->ledger );
        sxed->ledger = NULL;

        g_free (sxed->sxGUIDstr);
        sxed->sxGUIDstr = NULL;

        for ( i=0; i<(EX_CAL_NUM_MONTHS*31); i++ ) {
                g_free( sxed->cal_marks[i] );
        }
        g_free( sxed->cal_marks );

        g_free (sxed);
}

SchedXactionDialog*
gnc_ui_scheduled_xaction_dialog_create(void)
{
        SchedXactionDialog *sxd = NULL;
        GtkObject *sxdo;
        GtkWidget *button;
        GtkWidget *w;
        SchedXactionDialog *alreadyExisting = NULL;

        alreadyExisting = 
                (SchedXactionDialog*)
                gnc_find_first_gui_component( DIALOG_SCHEDXACTION_CM_CLASS,
                                              NULL,
                                              (gpointer)sxd );
        if ( alreadyExisting != NULL ) {
                gtk_window_present( GTK_WINDOW(alreadyExisting->dialog) );
                return alreadyExisting;
        }

        sxd = g_new0( SchedXactionDialog, 1 );

        sxd->gxml = gnc_glade_xml_new( SX_GLADE_FILE, SX_LIST_GLADE_NAME );
        sxd->dialog = glade_xml_get_widget( sxd->gxml, SX_LIST_GLADE_NAME );

        sxd->sxData = g_hash_table_new( NULL, NULL );

        sxdo = GTK_OBJECT(sxd->dialog);

        w = glade_xml_get_widget( sxd->gxml, SX_LIST_UPCOMING_FRAME );
        sxd->gdcal = GNC_DENSE_CAL( gnc_dense_cal_new() );
        gnc_dense_cal_set_months_per_col( sxd->gdcal, 4 );
        gnc_dense_cal_set_num_months( sxd->gdcal, 12 );
        gtk_container_add( GTK_CONTAINER(w), GTK_WIDGET(sxd->gdcal) );

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

        w = glade_xml_get_widget( sxd->gxml, SX_LIST );
        gtk_signal_connect(GTK_OBJECT(w), "select-row",
                           GTK_SIGNAL_FUNC(row_select_handler), sxd );
        gtk_signal_connect( GTK_OBJECT(w), "click-column",
                            GTK_SIGNAL_FUNC(gnc_sxd_row_click_handler), sxd );

        // gtk_clist_column_titles_active( GTK_CLIST( w ) );

        /* Default to sorting by ascending next-instance date. */
        sxd->currentSortCol = 2;
        sxd->currentSortType = GTK_SORT_ASCENDING;
        gnc_sxd_set_sort_compare( GTK_CLIST(w), sxd->currentSortCol );
        gtk_clist_set_auto_sort( GTK_CLIST(w), TRUE );

	gnc_restore_window_size(SX_LIST_GCONF_SECTION, GTK_WINDOW(sxd->dialog));

        gnc_register_gui_component( DIALOG_SCHEDXACTION_CM_CLASS,
                                    NULL, /* no refresh_handler */
                                    sxd_close_handler,
                                    sxd );

        schedXact_populate( sxd );

        gtk_widget_show_all(sxd->dialog);

        return sxd;
}

static
void
gnc_sxl_record_size( SchedXactionDialog *sxd )
{
	gnc_save_window_size(SX_LIST_GCONF_SECTION, GTK_WINDOW(sxd->dialog));
}

void
row_select_handler( GtkCList *clist,
                    gint row,
                    gint col,
                    GdkEventButton *event,
                    gpointer d )
{
        SchedXactionDialog *sxd;
        SchedXaction *sx;
       
        sxd   = (SchedXactionDialog*)d;

        if ( event == NULL ) {
                /* it could be a keypress */
                return;
        }

        switch ( event->type ) {
        case GDK_2BUTTON_PRESS:
                sx = (SchedXaction*)gtk_clist_get_row_data( clist, row );
                gnc_ui_scheduled_xaction_editor_dialog_create( sxd, sx, FALSE );
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
        GList *sxList;
        GtkCList *sx_clist;
        int i;

        sxList = gnc_book_get_schedxactions( gnc_get_current_book() );
        g_list_foreach( sxList, putSchedXactionInDialog, sxd );

        sx_clist = GTK_CLIST( glade_xml_get_widget( sxd->gxml,
                                                    SX_LIST ) );
        for ( i=0; i<3; i++ ) {
                gtk_clist_set_column_auto_resize( sx_clist, i, TRUE );
        }
}

static
gint
sxed_close_event( GtkDialog *dlg, gpointer ud )
{
        SchedXactionEditorDialog *sxed = (SchedXactionEditorDialog*)ud;

        /* We've already processed the SX, likely because of "ok" being
         * clicked. */
        if ( sxed->sx == NULL ) {
          return FALSE;
        }

        if ( ! sxed_confirmed_cancel( sxed ) ) {
                return TRUE;
        }
        return FALSE;
}

static
void
gnc_sxed_get_widgets( SchedXactionEditorDialog *sxed )
{
        GtkWidget *w;

        w = glade_xml_get_widget( sxed->gxml, SXED_NAME_ENTRY );
        sxed->nameEntry = GTK_EDITABLE(w);
        w = glade_xml_get_widget( sxed->gxml, SXED_LAST_OCCUR_LABEL );
        sxed->lastOccurLabel = GTK_LABEL(w);
        w = glade_xml_get_widget( sxed->gxml, AUTOCREATE_OPT );
        sxed->autocreateOpt = GTK_TOGGLE_BUTTON(w);
        w = glade_xml_get_widget( sxed->gxml, NOTIFY_OPT );
        sxed->notifyOpt = GTK_TOGGLE_BUTTON(w);
        w = glade_xml_get_widget( sxed->gxml, ADVANCE_OPT );
        sxed->advanceOpt = GTK_TOGGLE_BUTTON(w);
        w = glade_xml_get_widget( sxed->gxml, ADVANCE_DAYS_SPIN );
        sxed->advanceSpin = GTK_SPIN_BUTTON(w);
        w = glade_xml_get_widget( sxed->gxml, REMIND_OPT );
        sxed->remindOpt = GTK_TOGGLE_BUTTON(w);
        w = glade_xml_get_widget( sxed->gxml, REMIND_DAYS_SPIN );
        sxed->remindSpin = GTK_SPIN_BUTTON(w);

        w = glade_xml_get_widget( sxed->gxml, "rb_enddate" );
        sxed->optEndDate = GTK_TOGGLE_BUTTON(w);

        w = glade_xml_get_widget( sxed->gxml, "rb_noend" );
        sxed->optEndNone = GTK_TOGGLE_BUTTON(w);

        w = glade_xml_get_widget( sxed->gxml, "rb_num_occur" );
        sxed->optEndCount = GTK_TOGGLE_BUTTON(w);

        w = glade_xml_get_widget( sxed->gxml, END_ENTRY );
        sxed->endCountEntry = GTK_ENTRY(w);

        w = glade_xml_get_widget( sxed->gxml, REMAIN_ENTRY );
        sxed->endRemainEntry = GTK_ENTRY(w);

}

SchedXactionEditorDialog *
gnc_ui_scheduled_xaction_editor_dialog_create( SchedXactionDialog *sxd,
                                               SchedXaction *sx,
                                               gboolean newSX )
{
        SchedXactionEditorDialog *sxed;
        GtkWidget *button;
        int i;
        GList *dlgExists = NULL;

        static struct widgetSignalCallback {
                char     *name;
                char     *signal;
                void     (*fn)();
                gpointer objectData;
        } widgets[] = {
                { "ok_button",      "clicked", editor_ok_button_clicked,     NULL },
                { "cancel_button",  "clicked", editor_cancel_button_clicked, NULL },
                { "help_button",    "clicked", editor_help_button_clicked,   NULL },

                { "rb_noend",       "toggled", endgroup_rb_toggled,          GINT_TO_POINTER(END_NEVER_OPTION) },
                { "rb_enddate",     "toggled", endgroup_rb_toggled,          GINT_TO_POINTER(END_DATE_OPTION) },
                { "rb_num_occur",   "toggled", endgroup_rb_toggled,          GINT_TO_POINTER(NUM_OCCUR_OPTION) },

                { REMAIN_ENTRY ,    "changed", sxed_excal_update_adapt, NULL },

                { AUTOCREATE_OPT,   "toggled", autocreate_toggled,           NULL },
                { ADVANCE_OPT,      "toggled", advance_toggle,               (gpointer)ADVANCE_DAYS_SPIN },
                { REMIND_OPT,       "toggled", advance_toggle,               (gpointer)REMIND_DAYS_SPIN },

                { NULL,             NULL,      NULL,                         NULL }
        };

        dlgExists = gnc_find_gui_components( DIALOG_SCHEDXACTION_EDITOR_CM_CLASS,
                                             editor_component_sx_equality,
                                             sx );
        if ( dlgExists != NULL ) {
                DEBUG( "dialog already exists; using that one." );
                sxed = (SchedXactionEditorDialog*)dlgExists->data;
                gtk_window_present( GTK_WINDOW(sxed->dialog) );
                g_list_free( dlgExists );
                return sxed;
        }

        sxed         = g_new0( SchedXactionEditorDialog, 1 );
        sxed->gxml   = gnc_glade_xml_new( SX_GLADE_FILE,
                                        SX_EDITOR_GLADE_NAME );
        sxed->dialog = glade_xml_get_widget( sxed->gxml, SX_EDITOR_GLADE_NAME );

        sxed->sxd    = sxd;
        sxed->sx     = sx;
        sxed->newsxP = newSX;
        /* Setup dense-cal local mark storage */
        {
                sxed->cal_marks = g_new0( GDate*, EX_CAL_NUM_MONTHS * 31 );
                for( i=0; i<(EX_CAL_NUM_MONTHS * 31); i++ ) {
                        sxed->cal_marks[i] = g_date_new();
                }
                sxed->markId = -1;
        }

        /* Setup the end-date GNC widget */
        {
                GtkWidget *endDateBox =
                        glade_xml_get_widget( sxed->gxml, END_DATE_BOX );
                sxed->endDateEntry =
                        GNC_DATE_EDIT(gnc_date_edit_new( time(NULL),
                                                         FALSE, FALSE ));
                gtk_signal_connect( GTK_OBJECT(sxed->endDateEntry),
                                    "date-changed",
                                    GTK_SIGNAL_FUNC( sxed_excal_update_adapt ),
                                    sxed );
                gtk_box_pack_start( GTK_BOX(endDateBox),
                                    GTK_WIDGET(sxed->endDateEntry),
                                    TRUE, TRUE, 0 );
        }

        /* NOTE: this must occur before processing the widget list, defined
         * above, so the gpointers stored with the advance_ and remind_opts
         * are correct. */
        gnc_sxed_get_widgets( sxed );

        gnc_register_gui_component( DIALOG_SCHEDXACTION_EDITOR_CM_CLASS,
                                    NULL, /* no refresh handler */
                                    sxed_close_handler, /* file-static
                                                           close handler */
                                    sxed );

        gtk_signal_connect( GTK_OBJECT(sxed->dialog), "close",
                            GTK_SIGNAL_FUNC(sxed_close_event), sxed );
        gtk_signal_connect( GTK_OBJECT(sxed->dialog), "destroy",
                            GTK_SIGNAL_FUNC(scheduledxaction_editor_dialog_destroy),
                            sxed );

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

        /* For some reason the Glade-specified sensitivity settings are not
         * being honored... ? */
        gtk_widget_set_sensitive( GTK_WIDGET(sxed->notifyOpt), FALSE );
        gtk_widget_set_sensitive( GTK_WIDGET(sxed->advanceSpin), FALSE );
        gtk_widget_set_sensitive( GTK_WIDGET(sxed->remindSpin), FALSE );
        gtk_widget_set_sensitive( GTK_WIDGET(sxed->endCountEntry), FALSE );
        gtk_widget_set_sensitive( GTK_WIDGET(sxed->endRemainEntry), FALSE );

        gtk_editable_set_editable( GTK_EDITABLE(sxed->advanceSpin), TRUE );
        gtk_editable_set_editable( GTK_EDITABLE(sxed->remindSpin), TRUE );

	/* Allow resize */
        gtk_window_set_resizable (GTK_WINDOW(sxed->dialog), TRUE);

	gnc_restore_window_size(SXED_GCONF_SECTION, GTK_WINDOW(sxed->dialog));

        /* create the frequency-selection macrowidget and example
         * [dense-]calendar. */
        schedXact_editor_create_freq_sel( sxed );
        /* create the template-transaction ledger window */
        schedXact_editor_create_ledger( sxed );
        /* populate */
        schedXact_editor_populate( sxed );

	/* Do not call show_all here. Screws up the gtkuimanager code */
        gtk_widget_show(sxed->dialog);

	/* Refresh the cal and the ledger */
	gtk_widget_queue_resize( GTK_WIDGET( sxed->example_cal ) );
        gnc_ledger_display_refresh( sxed->ledger );

        return sxed;
}

static
void
gnc_sxed_record_size( SchedXactionEditorDialog *sxed )
{
	gnc_save_window_size( SXED_GCONF_SECTION, GTK_WINDOW(sxed->dialog) );
}

static
void
schedXact_editor_create_freq_sel( SchedXactionEditorDialog *sxed )
{
        GtkFrame *f;
        GtkBox *b;

        f = GTK_FRAME(glade_xml_get_widget( sxed->gxml, "gncfreq_frame" ));
        b = GTK_BOX( glade_xml_get_widget( sxed->gxml,
                                           "recur_box" ) );
        sxed->gncfreq =
                GNC_FREQUENCY( gnc_frequency_new( xaccSchedXactionGetFreqSpec(sxed->sx),
                                                  xaccSchedXactionGetStartDate(sxed->sx) ) );
        g_assert( sxed->gncfreq );
        gtk_signal_connect( GTK_OBJECT(sxed->gncfreq), "changed",
                            GTK_SIGNAL_FUNC(gnc_sxed_freq_changed),
                            sxed );
        gtk_container_add( GTK_CONTAINER(f), GTK_WIDGET(sxed->gncfreq) );

        f = GTK_FRAME(glade_xml_get_widget( sxed->gxml, "example_cal_frame" ));
        sxed->example_cal = GNC_DENSE_CAL(gnc_dense_cal_new());
        g_assert( sxed->example_cal );
        gnc_dense_cal_set_num_months( sxed->example_cal, EX_CAL_NUM_MONTHS );
        gnc_dense_cal_set_months_per_col( sxed->example_cal, EX_CAL_MO_PER_COL );
        gtk_container_add( GTK_CONTAINER(f), GTK_WIDGET(sxed->example_cal) );
	gtk_widget_show( GTK_WIDGET(sxed->example_cal) );
}

static
void
schedXact_editor_create_ledger( SchedXactionEditorDialog *sxed )
{
        GtkFrame *tempxaction_frame;
        SplitRegister *splitreg;
        GtkWidget *main_vbox;

        tempxaction_frame =
                GTK_FRAME( glade_xml_get_widget( sxed->gxml,
                                                 "tempxaction_frame" ) );

	/* Create the ledger */
        /* THREAD-UNSAFE */
        sxed->sxGUIDstr = g_strdup( guid_to_string(
                                        xaccSchedXactionGetGUID(sxed->sx) ) );
        sxed->ledger = gnc_ledger_display_template_gl( sxed->sxGUIDstr );
        splitreg = gnc_ledger_display_get_split_register( sxed->ledger );

	/* First the embedded window */
        main_vbox = glade_xml_get_widget( sxed->gxml, "register_vbox" );
	sxed->embed_window =
	  gnc_embedded_window_new("SXWindowActions",
				     gnc_sxed_menu_entries,
				     gnc_sxed_menu_n_entries,
				     "gnc-sxed-window-ui.xml",
				     sxed->dialog,
				     FALSE, /* no accelerators */
				     sxed);
	gtk_box_pack_start (GTK_BOX (main_vbox), GTK_WIDGET(sxed->embed_window),
			    TRUE, TRUE, 0);

	/* Now create the register plugin page. */
	sxed->plugin_page = gnc_plugin_page_register_new_ledger (sxed->ledger);
	gnc_plugin_page_set_ui_description (sxed->plugin_page,
					    "gnc-plugin-page-sxregister-ui.xml");
	gnc_plugin_page_register_set_options (sxed->plugin_page,
					      SXED_GCONF_SECTION,
					      KEY_NUMBER_OF_ROWS,
					      NUM_LEDGER_LINES_DEFAULT,
					      (CAP_JUMP | CAP_SCHEDULE) );
	gnc_embedded_window_open_page (sxed->embed_window, sxed->plugin_page);

        /* configure... */
        /* don't use double-line */
        gnc_split_register_config(splitreg,
                                  splitreg->type, splitreg->style,
                                  FALSE);

        /* don't show present/future divider [by definition, not necessary] */
        gnc_split_register_show_present_divider( splitreg, FALSE );
}

static
void
schedXact_editor_populate( SchedXactionEditorDialog *sxed )
{
        char *name;
        time_t tmpDate;
        SplitRegister *splitReg;
        GString *tmpgStr;
        struct tm *tmpTm;
        GDate *gd;
        gint daysInAdvance;
        gboolean autoCreateState, notifyState;

        name = xaccSchedXactionGetName(sxed->sx);
        if ( name != NULL ) {
                gtk_entry_set_text( GTK_ENTRY(sxed->nameEntry), name  );
        }
        {
                gd = xaccSchedXactionGetLastOccurDate( sxed->sx );
                if ( g_date_valid( gd ) ) {
                        gchar dateBuf[ MAX_DATE_LENGTH+1 ];
                        qof_print_gdate( dateBuf,MAX_DATE_LENGTH, gd );
                        gtk_label_set_text( sxed->lastOccurLabel, dateBuf );
                } else {
                        gtk_label_set_text( sxed->lastOccurLabel, _( "(never)" ) );
                }
                gd = NULL;
        }

        gd = xaccSchedXactionGetEndDate( sxed->sx );
        if ( g_date_valid( gd ) ) {
                gtk_toggle_button_set_active( sxed->optEndDate, TRUE );
                /* fill in date data. */
                tmpTm = g_new0( struct tm, 1 );
                g_date_to_struct_tm( gd, tmpTm );
                tmpDate = mktime( tmpTm );
                g_free( tmpTm );
                gnc_date_edit_set_time( sxed->endDateEntry, tmpDate );

                set_endgroup_toggle_states( sxed, END_DATE );
        } else if ( xaccSchedXactionHasOccurDef( sxed->sx ) ) {
                gint numOccur = xaccSchedXactionGetNumOccur( sxed->sx );
                gint numRemain = xaccSchedXactionGetRemOccur( sxed->sx );

                gtk_toggle_button_set_active( sxed->optEndCount, TRUE );

                tmpgStr = g_string_sized_new(5);
                g_string_printf( tmpgStr, "%d", numOccur );
                gtk_entry_set_text( sxed->endCountEntry, tmpgStr->str );
                g_string_free( tmpgStr, TRUE );

                tmpgStr = g_string_sized_new(5);
                g_string_printf( tmpgStr, "%d", numRemain );
                gtk_entry_set_text( sxed->endRemainEntry, tmpgStr->str );
                g_string_free( tmpgStr, TRUE );

                set_endgroup_toggle_states( sxed, END_OCCUR );
        } else {
                gtk_toggle_button_set_active( sxed->optEndNone, TRUE );
                set_endgroup_toggle_states( sxed, END_NEVER );
        }

        /* Do auto-create/notify setup */
        if ( sxed->newsxP ) {
                autoCreateState =
		  gnc_gconf_get_bool( SXED_GCONF_SECTION, KEY_CREATE_AUTO, NULL );
                notifyState =
		  gnc_gconf_get_bool( SXED_GCONF_SECTION, KEY_NOTIFY, NULL );
        } else {
                xaccSchedXactionGetAutoCreate( sxed->sx,
                                               &autoCreateState,
                                               &notifyState );
        }
        gtk_toggle_button_set_active( sxed->autocreateOpt, autoCreateState );
        if ( ! autoCreateState ) {
                notifyState = FALSE;
        }
        gtk_toggle_button_set_active( sxed->notifyOpt, notifyState );


        /* Do days-in-advance-to-create widget[s] setup. */
        if ( sxed->newsxP ) {
                daysInAdvance =
		  gnc_gconf_get_float( SXED_GCONF_SECTION, KEY_CREATE_DAYS, NULL );
        } else {
                daysInAdvance =
                        xaccSchedXactionGetAdvanceCreation( sxed->sx );
        }
        if ( daysInAdvance != 0 ) {
                gtk_toggle_button_set_active( sxed->advanceOpt, TRUE );
                gtk_spin_button_set_value( sxed->advanceSpin,
                                           (gfloat)daysInAdvance );
        }

        /* Do days-in-advance-to-remind widget[s] setup. */
        if ( sxed->newsxP ) {
                daysInAdvance =
		  gnc_gconf_get_float( SXED_GCONF_SECTION, KEY_REMIND_DAYS, NULL );
        } else {
                daysInAdvance =
                        xaccSchedXactionGetAdvanceReminder( sxed->sx );
        }
        if ( daysInAdvance != 0 ) {
                gtk_toggle_button_set_active( sxed->remindOpt, TRUE );
                gtk_spin_button_set_value( sxed->remindSpin,
                                           (gfloat)daysInAdvance );
        }

        if ( sxed->newsxP ) {
                gnc_sx_set_instance_count( sxed->sx, 1 );
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

        /* Update the example cal */
        gnc_sxed_update_cal( sxed );
}

static
void
set_endgroup_toggle_states( SchedXactionEditorDialog *sxed, EndType type )
{
        gtk_widget_set_sensitive( GTK_WIDGET(sxed->endDateEntry),   (type == END_DATE) );
        gtk_widget_set_sensitive( GTK_WIDGET(sxed->endCountEntry),  (type == END_OCCUR) );
        gtk_widget_set_sensitive( GTK_WIDGET(sxed->endRemainEntry), (type == END_OCCUR) );
}

static
void
new_button_clicked( GtkButton *b, gpointer d )
{
        SchedXactionDialog        *sxd;
        FreqSpec *fs;
        GDate *gd;
        SchedXaction        *tmpSX =
                xaccSchedXactionMalloc( gnc_get_current_book ());
        SchedXactionEditorDialog *sxed;

        /* Give decent initial FreqSpec for SX */
        fs = xaccSchedXactionGetFreqSpec( tmpSX );
        gd = g_date_new();
        g_date_set_time( gd, time(NULL) );
        xaccFreqSpecSetMonthly( fs, gd, 1 );
        xaccFreqSpecSetUIType ( fs, UIFREQ_MONTHLY );
        g_date_free( gd );
        
        sxd = (SchedXactionDialog*)d;
        sxed = gnc_ui_scheduled_xaction_editor_dialog_create( sxd, tmpSX,
                                                              TRUE /* newSX */ );
}

static
void
edit_button_clicked( GtkButton *b, gpointer d )
{
        GList *sel;
        GtkCList *cl;
        int row;
        SchedXactionDialog *sxd;
        SchedXaction *sx;
        SchedXactionEditorDialog *sxed;

        sxd = (SchedXactionDialog*)d;
        cl = GTK_CLIST(glade_xml_get_widget( sxd->gxml, SX_LIST ));
        for( sel = cl->selection; sel; sel = g_list_next(sel) ) {
                row = GPOINTER_TO_INT(sel->data);
                /* get the clist row for this listitem */
                sx = (SchedXaction*)gtk_clist_get_row_data( cl, row );
                /* get the object UD */
                sxed = gnc_ui_scheduled_xaction_editor_dialog_create( sxd, sx, FALSE );
        }
}

static void
delete_button_clicked( GtkButton *b, gpointer d )
{
        GNCBook *book;
        GtkCList *cl;
        GList *sel, *sxList, *beingEditedList, *l;
        SchedXactionDialog *sxd;
        char *beingEditedMessage =
          _( "The following transactions are presently being edited;\n"
             "are you sure you want to delete them?" );
        char *confirmMessage =
          _( "Delete the selected Scheduled Transactions?" );
        GString  *realConfDelOpenMsg, *realConfDeleteMsg;
        SchedXaction *sx;
        gboolean destroyOpenedResult = FALSE;

        sxd = (SchedXactionDialog*)d;

        cl = GTK_CLIST(glade_xml_get_widget( sxd->gxml, SX_LIST ));
        sel = cl->selection;

        if ( !sel ) {
                return;
        }

        realConfDeleteMsg = g_string_new( confirmMessage );
        realConfDelOpenMsg = g_string_new( beingEditedMessage );
        beingEditedList = NULL;
        for ( ; sel ; sel = sel->next ) {
                sx = (SchedXaction*)gtk_clist_get_row_data( cl, GPOINTER_TO_INT(sel->data));
                g_string_append_printf( realConfDeleteMsg, "\n\"%s\"",
                                        xaccSchedXactionGetName( sx ) );
                if ( (l = gnc_find_gui_components( DIALOG_SCHEDXACTION_EDITOR_CM_CLASS,
                                                   editor_component_sx_equality,
                                                   sx )) ) {
                        beingEditedList = g_list_append( beingEditedList, (gpointer)l );
                        g_string_append_printf( realConfDelOpenMsg, "\n\"%s\"",
                                                xaccSchedXactionGetName( sx ) );
                }
        }

        if ( g_list_length( beingEditedList ) > 0 ) {
                /* Figure out the user's disposition [toward the opened
                 * transactions], but if it's true, don't act on it until
                 * they confirm they actually want to do the deletion
                 * generically.  If it's false, cleanup and return. */
                if ( ! (destroyOpenedResult =
                        gnc_verify_dialog( sxd->dialog, FALSE, "%s",
					   realConfDelOpenMsg->str )) ) {
                        for ( l = beingEditedList; l; l = l->next ) {
                                g_list_free( (GList*)l->data );
                        }
                        g_list_free( beingEditedList );
                        goto cleanupStrings;
                        return; /* unreachable, but clearer. */
                }
        }

        if ( gnc_verify_dialog( sxd->dialog, FALSE, "%s",
				realConfDeleteMsg->str ) ) {
                /* Close the being-edited transactions. */
                if ( destroyOpenedResult ) {
                        GList *component;
                        for ( l = beingEditedList; l; l = l->next ) {
                                SplitRegister *reg;
                                component = (GList*)l->data;
                                /* We'd like to force the cancellation of
                                 * ledger/other changes, here. */
                                reg = gnc_ledger_display_get_split_register(
                                        ((SchedXactionEditorDialog*)component
                                         ->data)
                                        ->ledger );
                                gnc_split_register_cancel_cursor_trans_changes(
                                        reg );
                                editor_cancel_button_clicked(
                                        NULL,
                                        (SchedXactionEditorDialog*)component
                                        ->data );
                                g_list_free( component );
                        }
                        g_list_free( beingEditedList );
                }

                /* Now, actually do the deletions... */
                book = gnc_get_current_book ();
                sxList = gnc_book_get_schedxactions( book );
                for ( sel = cl->selection; sel; sel = sel->next ) {
                        SxRuntimeInfo *sxri;
                        SxRuntimeInfo **p_sxri = &sxri;
                        gpointer unused;
                        gboolean foundP;

                        sx = (SchedXaction*)gtk_clist_get_row_data( cl, GPOINTER_TO_INT(sel->data));
                        sxList = g_list_remove( sxList, (gpointer)sx );
                        foundP = g_hash_table_lookup_extended( sxd->sxData, sx,
                                                               &unused,
                                                               (gpointer*)p_sxri );
                        g_assert( foundP );
                        if ( sxri->markTag != -1 ) {
                                gnc_dense_cal_mark_remove( sxd->gdcal, sxri->markTag );
                        }
                        g_hash_table_remove( sxd->sxData, sx );
                        xaccSchedXactionFree( sx );
                }
                gnc_book_set_schedxactions( book, sxList );

                gtk_clist_freeze( cl );
                /* Remove the selected and deleted rows from the clist in
                 * reverse order so each index is valid. */
                sel = g_list_copy( cl->selection );
                sel = g_list_reverse( sel );
                gtk_clist_unselect_all( cl );
                for ( ; sel; sel = sel->next ) {
                        gtk_clist_remove( cl, GPOINTER_TO_INT(sel->data) );
                }
                g_list_free( sel );
                sel = NULL;
                gtk_clist_thaw( cl );
        }

 cleanupStrings:
        g_string_free( realConfDeleteMsg, TRUE );
        g_string_free( realConfDelOpenMsg, TRUE );
}

static
void
endgroup_rb_toggled( GtkButton *b, gpointer d )
{
        /* figure out which one */
        SchedXactionEditorDialog *sxed;
        gint id;

        sxed = (SchedXactionEditorDialog*)d;
        id = GPOINTER_TO_INT(gtk_object_get_data( GTK_OBJECT(b),
                                                  "whichOneAmI" ));

        switch (id) {
        case END_NEVER_OPTION:
                set_endgroup_toggle_states( sxed, END_NEVER );
                break;
        case END_DATE_OPTION:
                set_endgroup_toggle_states( sxed, END_DATE );
                break;
        case NUM_OCCUR_OPTION:
                set_endgroup_toggle_states( sxed, END_OCCUR );
                break;
        default:
                g_error( "Unknown id %d", id );
                break;
        }

        gnc_sxed_update_cal( sxed );
}

/**
 * This is a copy of more complex code from dialog-sxsincelast.c.  They
 * should probably be combined into a single useful function somewhere.
 **/
static
void
generate_instances( SchedXaction *sx,
                    GDate *end, GList **instanceList )
{
        GDate gd, *gdToReturn;
        void *seqStateData;

        /* Process valid next instances */
        seqStateData = gnc_sx_create_temporal_state( sx );
        gd = xaccSchedXactionGetNextInstance( sx, seqStateData );
        while ( g_date_valid(&gd)
                && g_date_compare( &gd, end ) <= 0 ) {

                gdToReturn = g_date_new();
                *gdToReturn = gd;
                *instanceList = g_list_append( *instanceList, gdToReturn );

                gnc_sx_incr_temporal_state( sx, seqStateData );
                gd = xaccSchedXactionGetInstanceAfter( sx, &gd, seqStateData );
        }
        gnc_sx_destroy_temporal_state( seqStateData );
        seqStateData = NULL;
}

static
void
_gnc_sxd_free_dates( gpointer data, gpointer user_data )
{
        g_date_free( (GDate*)data );
}

static
SxRuntimeInfo*
_new_sx_runtime_info( SchedXaction *sx )
{
        SxRuntimeInfo *sxri;

        sxri = g_new0( SxRuntimeInfo, 1 );
        sxri->sx      = sx;
        sxri->row     = -1;
        sxri->markTag = -1;
        return sxri;
}

static
void
putSchedXactionInDialog( gpointer data, gpointer user_data )
{
        SchedXaction *sx;
        SchedXactionDialog *sxd;
        GtkCList *clist;
        char *text[3];
        GString *freqStr;
        GString *nextDate;
        int i;
        GDate *nextInstDate = NULL, *calEndDate;
        int instArraySize;
        GDate **instArray;
        GList *instList;
        gpointer unused;
        SxRuntimeInfo *sxri = NULL;
        SxRuntimeInfo **p_sxri = &sxri;
        gboolean foundP;
        gint gdcMarkTag;
        gint row;

        sx = (SchedXaction*)data;
        sxd = (SchedXactionDialog*)user_data;

        freqStr = g_string_new( "" );
        nextDate = g_string_new( "" );

        xaccFreqSpecGetFreqStr( xaccSchedXactionGetFreqSpec(sx), freqStr );

        calEndDate = g_date_new_dmy( 1,
                                     gnc_dense_cal_get_month(sxd->gdcal),
                                     gnc_dense_cal_get_year(sxd->gdcal) );
        g_date_add_months( calEndDate,
                           gnc_dense_cal_get_num_months(sxd->gdcal) );

        instList = NULL;
        generate_instances( sx, calEndDate, &instList );
        g_date_free( calEndDate );

        if ( instList == NULL ) {
                /* This was a bug [#90326]; while we do want to generate
                 * instances within the visible calendar range, we also want
                 * to generate the first, next SX instance regardless of the
                 * calendar range.  Thus, if the generate_instances above
                 * returns nothing, double-check with the SX. */
                nextInstDate = g_date_new();
                *nextInstDate = xaccSchedXactionGetNextInstance( sx, NULL );
                if ( g_date_valid( nextInstDate ) ) {
                        instList = g_list_append( instList,
                                                  (gpointer)nextInstDate );
                }
        }

        if ( instList == NULL ) {
                g_string_printf( nextDate, _("Not scheduled") );
        } else {
                char tmpBuf[ MAX_DATE_LENGTH+1 ];
                char dowBuf[ 25 ]; /* <- FIXME: appropriate length? */
                nextInstDate = (GDate*)instList->data;
                qof_print_gdate( tmpBuf, MAX_DATE_LENGTH, nextInstDate );
                g_date_strftime( dowBuf, 25, "%A", nextInstDate );
                g_string_printf( nextDate, "%s (%s)", tmpBuf, dowBuf );
        }

        /* Add markings to GncDenseCal */
        gdcMarkTag = -1;
        if ( instList != NULL ) {
                GList *l;
                FreqSpec *fs;
                GString *freqDesc;

                instArraySize = g_list_length( instList );
                instArray = g_new0( GDate*, instArraySize );
                for ( i=0, l=instList; l; l = l->next ) {
                        instArray[i++] = (GDate*)l->data;
                }
                freqDesc = g_string_sized_new(64);
                fs = xaccSchedXactionGetFreqSpec(sx);
                xaccFreqSpecGetFreqStr(fs, freqDesc );
                gdcMarkTag = gnc_dense_cal_mark( sxd->gdcal,
                                                 instArraySize, instArray,
                                                 xaccSchedXactionGetName(sx),
                                                 freqDesc->str );
                g_string_free( freqDesc, TRUE );
                g_free( instArray );
                g_list_foreach( instList, _gnc_sxd_free_dates, NULL );
                g_list_free( instList );
                nextInstDate = NULL;
        }

        foundP = g_hash_table_lookup_extended( sxd->sxData,
                                               (gpointer)sx,
                                               &unused,
                                               (gpointer*)p_sxri );
        if ( ! foundP )
        {
                // new SX -- create runtime storage for it
                sxri = _new_sx_runtime_info( sx );
                sxri->markTag = gdcMarkTag;
        }
        else
        {
                // existing SX; remove it's 
                if ( sxri->markTag != -1 ) {
                        gnc_dense_cal_mark_remove( sxd->gdcal, sxri->markTag );
                        sxri->markTag = gdcMarkTag;
                }
        }


        text[0] = xaccSchedXactionGetName( sx );
        text[1] = freqStr->str;
        text[2] = nextDate->str;

        clist = GTK_CLIST( glade_xml_get_widget( sxd->gxml, SX_LIST ) );
        gtk_clist_freeze( clist );

        row = gtk_clist_find_row_from_data( clist, (gpointer)sx );
        if ( sxri->row == -1 ) {
                /* new item to be inserted */
                sxri->row = gtk_clist_append( clist, text );
                gtk_clist_set_row_data( clist, sxri->row, sx );
        } else {
                for ( i=0; i<3; i++ ) {
                        gtk_clist_set_text( clist, sxri->row, i, text[i] );
                }
        }
        gtk_clist_sort( clist );
        gtk_clist_thaw( clist );
        g_hash_table_insert( sxd->sxData, (gpointer)sx, (gpointer)sxri );
        sxri = NULL;

        g_string_free( freqStr,  TRUE );
        g_string_free( nextDate, TRUE );
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
        const char *message =
                _("The current template transaction "
                  "has been changed.\n"
                  "Would you like to record the changes?");
        
        reg = gnc_ledger_display_get_split_register (sxed->ledger);
        pending_changes = gnc_split_register_changed (reg);
        if (!pending_changes) {
                return;
        }

        if (gnc_verify_dialog(sxed->dialog, TRUE, message)) {
                Transaction *trans;
                trans = gnc_split_register_get_current_trans( reg );
                if ( !gnc_split_register_save( reg, TRUE ) )
                        return;
                
                gnc_split_register_redraw( reg );
        } else {
                gnc_split_register_cancel_cursor_trans_changes (reg);
        }
}

static gboolean
editor_component_sx_equality( gpointer find_data,
                              gpointer user_data )
{
        return ( (SchedXaction*)find_data
                 == ((SchedXactionEditorDialog*)user_data)->sx );
}

static
void
gnc_sxd_row_click_handler( GtkCList *clist,
                           gint col,
                           gpointer ud )
{
        SchedXactionDialog *sxd = (SchedXactionDialog*)ud;

        if ( col == sxd->currentSortCol ) {
                g_assert( sxd->currentSortType == GTK_SORT_ASCENDING
                          || sxd->currentSortType == GTK_SORT_DESCENDING );
                switch ( sxd->currentSortType ) {
                case GTK_SORT_ASCENDING:
                        sxd->currentSortType = GTK_SORT_DESCENDING;
                        break;
                case GTK_SORT_DESCENDING:
                        sxd->currentSortType = GTK_SORT_ASCENDING;
                        break;
                default:
                        PERR( "Unknown current sort type %d",
                              sxd->currentSortType );
                }
                /* By defn, the current sort_compare method is correct. */
                gtk_clist_set_sort_column( clist, col );
                gtk_clist_set_sort_type( clist, sxd->currentSortType );
                gtk_clist_sort( clist );
                return;
        }

        sxd->currentSortCol = col;
        gnc_sxd_set_sort_compare( clist, sxd->currentSortCol );
        sxd->currentSortType = GTK_SORT_ASCENDING;
        gtk_clist_set_sort_column( clist, sxd->currentSortCol );
        gtk_clist_set_sort_type( clist, sxd->currentSortType );
        gtk_clist_sort( clist );
}

static
gint
gnc_sxd_clist_compare_sx_name( GtkCList *cl, gconstpointer a, gconstpointer b )
{
        SchedXaction *sxa, *sxb;

        sxa = (SchedXaction*)(((GtkCListRow*)a)->data);
        sxb = (SchedXaction*)(((GtkCListRow*)b)->data);
        g_assert( sxa || sxb );
        if ( !sxa ) {
                return 1;
        }
        if ( !sxb ) {
                return -1;
        }
        return strcmp( xaccSchedXactionGetName( sxa ),
                       xaccSchedXactionGetName( sxb ) );
}

static
gint
gnc_sxd_clist_compare_sx_freq( GtkCList *cl,
                               gconstpointer a,
                               gconstpointer b )
{
        SchedXaction *sxa, *sxb;

        g_assert( a || b );
        if ( !a ) return 1;
        if ( !b ) return -1;
        sxa = (SchedXaction*)((GtkCListRow*)a)->data;
        sxb = (SchedXaction*)((GtkCListRow*)b)->data;
        g_assert( sxa || sxb );
        if ( !sxa ) return 1;
        if ( !sxb ) return -1;
        return gnc_freq_spec_compare( xaccSchedXactionGetFreqSpec( sxa ),
                                      xaccSchedXactionGetFreqSpec( sxb ) );
}

static
gint
gnc_sxd_clist_compare_sx_next_occur( GtkCList *cl,
                                     gconstpointer a,
                                     gconstpointer b )
{
        SchedXaction *sxa, *sxb;
        GDate gda, gdb;

        sxa = (SchedXaction*)((GtkCListRow*)a)->data;
        sxb = (SchedXaction*)((GtkCListRow*)b)->data;

        g_assert( sxa || sxb );
        if ( !sxa ) {
                return 1;
        }
        if ( !sxb ) {
                return -1;
        }
        g_assert( sxa && sxb );

        gda = xaccSchedXactionGetNextInstance( sxa, NULL );
        gdb = xaccSchedXactionGetNextInstance( sxb, NULL );

        if ( ! ( g_date_valid(&gda) && g_date_valid(&gdb) ) ) {
                return 0;
        }
        if ( !g_date_valid(&gda) ) {
                return 1;
        }
        if ( !g_date_valid(&gdb) ) {
                return -1;
        }
        return g_date_compare( &gda, &gdb );
}


static
void
gnc_sxd_set_sort_compare( GtkCList *cl, gint col )
{
        gint (*fn)( GtkCList *, gconstpointer, gconstpointer );

        fn = NULL;
        switch ( col ) {
        case 0: /* SX name */
                fn = gnc_sxd_clist_compare_sx_name;
                break;
        case 1: /* SX frequency */
                fn = gnc_sxd_clist_compare_sx_freq;
                break;
        case 2: /* next-occur date */
                fn = gnc_sxd_clist_compare_sx_next_occur;
                break;
        default: /* ?? */
                DEBUG( "invalid column value %d", col );
                g_assert( FALSE );
        }
        gtk_clist_set_compare_func( cl, NULL );
        gtk_clist_set_compare_func( cl, fn );
}

typedef enum { NO_END, DATE_END, COUNT_END } END_TYPE;

static
void
gnc_sxed_update_cal( SchedXactionEditorDialog *sxed )
{
        int i;
        FreqSpec *fs;
        GDate d;
        END_TYPE endType;
        GDate endDate;
        int numRemain;
	gchar *text;

        endType = NO_END;
        numRemain = -1;
        /* figure out the end restriction */
        if ( gtk_toggle_button_get_active( sxed->optEndDate ) ) {
                time_t tt;
                struct tm *tmpTm;
                endType = DATE_END;
                tt = gnc_date_edit_get_date( sxed->endDateEntry );
                tmpTm = g_new0( struct tm, 1 );
                *tmpTm = *(localtime( &tt ));
                g_date_set_day( &endDate, tmpTm->tm_mday );
                g_date_set_month( &endDate, tmpTm->tm_mon+1 );
                g_date_set_year( &endDate, tmpTm->tm_year + 1900 );
                g_free( tmpTm );
        } else if ( gtk_toggle_button_get_active( sxed->optEndNone ) ) {
                endType = NO_END;
        } else if ( gtk_toggle_button_get_active( sxed->optEndCount ) ) {
                endType = COUNT_END;
		text = gtk_editable_get_chars( GTK_EDITABLE (sxed->endRemainEntry),
					       0, -1 );
		sscanf (text, "%d", &numRemain);
		g_free (text);
        } else {
                g_assert( FALSE );
        }

        if ( sxed->markId != -1 ) {
                gnc_dense_cal_mark_remove( sxed->example_cal, sxed->markId );
                sxed->markId = -1;
        }

        fs = xaccFreqSpecMalloc( gnc_get_current_book() );
        gnc_frequency_save_state( sxed->gncfreq, fs, &d );
        g_date_subtract_days( &d, 1 );
        xaccFreqSpecGetNextInstance( fs, &d, &d );

        /* Deal with the fact that this SX may have been run before [the
         * calendar should only show upcoming instances]... */
        {
                GDate *lastInst;

                lastInst = xaccSchedXactionGetLastOccurDate( sxed->sx );
                if ( g_date_valid( lastInst )
                     && g_date_valid( &d )
                     && g_date_compare( lastInst, &d ) != 0 ) {
                        d = *lastInst;
                        xaccFreqSpecGetNextInstance( fs, &d, &d );
                }
        }

        if ( !g_date_valid( &d ) ) {
                /* Nothing to do. */
                xaccFreqSpecFree( fs );
                return;
        }

        i = 0;
        gnc_dense_cal_set_month( sxed->example_cal, g_date_get_month( &d ) );
        gnc_dense_cal_set_year(  sxed->example_cal, g_date_get_year( &d ) );
        while ( (i < EX_CAL_NUM_MONTHS * 31)
                && g_date_valid( &d )
                /* Restrict based on end date */
                && ( endType == NO_END
                     || ( endType == DATE_END
                          && g_date_compare( &d, &endDate ) <= 0 )
                     || ( endType == COUNT_END
                          && i < numRemain ) ) ) {
                *(sxed->cal_marks[i++]) = d;
                xaccFreqSpecGetNextInstance( fs, &d, &d );
        }
        if ( i <= 0 ) {
                xaccFreqSpecFree( fs );
                return;
        }

        { 
                gchar *name;
                GString *info;

                name = gtk_editable_get_chars( sxed->nameEntry, 0, -1 );
                if ( strlen( name ) == 0 ) {
                        g_free(name);
                        name = NULL;
                }
                info = g_string_sized_new( 16 );
                xaccFreqSpecGetFreqStr( fs, info );
                sxed->markId = gnc_dense_cal_mark( sxed->example_cal, i,
                                                   sxed->cal_marks,
                                                   name, info->str );
                gtk_widget_queue_draw( GTK_WIDGET( sxed->example_cal ) );

                g_string_free( info, TRUE );
                if ( name != NULL )
                {
                        g_free( name );
                }
        }

        xaccFreqSpecFree( fs );
}

static
void
gnc_sxed_freq_changed( GNCFrequency *gf, gpointer ud )
{
        gnc_sxed_update_cal( (SchedXactionEditorDialog*)ud );
}

static
void
sxed_excal_update_adapt( GtkObject *o, gpointer ud )
{
        gnc_sxed_update_cal( (SchedXactionEditorDialog*)ud );
}

/* Command callbacks */
static void
gnc_sxed_cmd_edit_cut (GtkAction *action, SchedXactionEditorDialog *sxed)
{
}

static void
gnc_sxed_cmd_edit_copy (GtkAction *action, SchedXactionEditorDialog *sxed)
{
}

static void
gnc_sxed_cmd_edit_paste (GtkAction *action, SchedXactionEditorDialog *sxed)
{
}


void on_sx_check_toggled (GtkWidget *togglebutton, gpointer user_data);

void
on_sx_check_toggled (GtkWidget *togglebutton,
		     gpointer user_data)
{
  GtkWidget *widget;
  gboolean create; // , notify;

  /* The gnc_glade_lookup_widget() function works because all of these
   * widgets come from the same glade file. */
  widget = gnc_glade_lookup_widget(togglebutton,
	"gconf/dialogs/scheduled_trans/transaction_editor/create_auto");
  create = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget));
  widget = gnc_glade_lookup_widget(togglebutton,
	"gconf/dialogs/scheduled_trans/transaction_editor/notify");
  gtk_widget_set_sensitive(widget, create);
}


void
gnc_ui_sx_initialize (void)
{
  gnc_hook_add_dangler(HOOK_BOOK_OPENED,
		       (GFunc)gnc_sx_sxsincelast_book_opened, NULL);
  gnc_preferences_add_page (SX_GLADE_FILE,
			    "sx_prefs",
			    _("Scheduled Transactions"));
}
